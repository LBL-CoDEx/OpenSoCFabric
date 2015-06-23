package OpenSoC

import Chisel._
import FixedPoint._
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.util.Random

//-------------------------------
// Packet format
// All packet fields are the same width and must be a power of 2
// It is currently assumed that packet width is equal to flit payload width
//
//  Packet: 
// ----------------------------------------------------
// | Source Address       							  | 
// ----------------------------------------------------
// | Destination Address   							  |
// ----------------------------------------------------
// | Total Length | Payload Length | Additional Flags |
// ----------------------------------------------------
// | PacketID | Command | Command Options 			  | 
// ----------------------------------------------------
// |   ------- RESERVED FOR DEBUG -------			  |
// ----------------------------------------------------
// | Payload Phase 0								  |
// ----------------------------------------------------
//                ...
// ----------------------------------------------------
// | Payload Phase N							 	  |
// ----------------------------------------------------


object PacketFieldIndex extends Enumeration {
	val totalLength		= 0
	val payloadLength	= 1
	val additionalFlags	= 2
	val packetID		= 0
	val command			= 1
	val commandOptions	= 2
}

class Packet (parms: Parameters) extends Bundle {
	val packetWidth				= parms.get[Int]("packetWidth")
	val packetLength			= parms.get[Int]("packetMaxLength")
	val	packetControlFieldCount = 5
	val packetLengthFieldCount 	= 3
	val packetCommandFieldCount = 3
	val	sourceAddress			= UInt(INPUT, width = packetWidth)
	val	destAddress				= UInt(INPUT, width = packetWidth)
	val	length					= Vec.fill(packetLengthFieldCount) { UInt(INPUT, width=packetWidth) }
	val	command					= Vec.fill(packetCommandFieldCount){ UInt(INPUT, width = packetWidth) }
	val debug					= UInt(INPUT, width = packetWidth)
	val	payload					= Vec.fill(packetLength - packetControlFieldCount){ UInt(INPUT, width = packetWidth) }

	override def clone = { new Packet(parms).asInstanceOf[this.type] }
}

abstract class InputToFlit[T<: Data](parms : Parameters, tGen : Parameters => T )extends Module(parms) {

	val io = new Bundle {
		val packet : T = tGen(parms).asInput
		val packetReady	= Bool(OUTPUT)
		val packetValid	= Bool(INPUT)
		val flit		= new Flit(parms).asOutput
		val flitReady	= Bool(INPUT)
		val flitValid	= Bool(OUTPUT)
	}
}

class FlitToFlit(parms: Parameters) extends InputToFlit[Flit](parms, p => new Flit(parms) ) {

	
	io.flit				:= io.packet
	io.flitValid		:= io.packetValid
	io.packetReady		:= io.flitReady

}

//Converts a single packet to multiple flits - expected operation is that packet and packetValid must be driven until packetDone is driven high
class PacketToFlit(parms: Parameters) extends InputToFlit[Packet](parms, p => new Packet(parms) ) {

	val maxPacketLength = parms.get[Int]("packetMaxLength")
	val packetWidth		= parms.get[Int]("packetWidth")
	val packetIDWidth	= parms.get[Int]("packetIDWidth")
	val payloadWidth	= parms.get[Int]("payloadWidth")
	val flitIDWidth		= parms.get[Int]("flitIDWidth")
	val packetTypeWidth	= parms.get[Int]("packetTypeWidth")
	val destCordWidth	= parms.get[Int]("destCordWidth")
	val numVCs			= parms.get[Int]("numVCs")
	val Dim				= parms.get[Int]("TopologyDimension")
	val C 				= parms.get[Int]("Concentration") // Processors (endpoints) per router.

	val flitWidth 		= Flit.fromBits(UInt(0), parms).getWidth()

	val headBundle2Flit = Chisel.Module( new HeadBundle2Flit(parms ) )
	val bodyBundle2Flit	= Chisel.Module( new BodyBundle2Flit(parms ) )


	//Create states
	val s_idle 	:: s_createHeadFlit :: s_sourceAddress :: s_destAddress	:: s_length	:: s_pktIDcommand :: s_debug :: s_payload :: Nil = Enum(UInt(), 8)

	val state			= Reg(init = s_idle)
	val payloadPhase	= Reg(init = UInt(0, width=log2Up(maxPacketLength)))
	val queue 			= Chisel.Module( new Chisel.Queue(new Packet(parms), 2) )
	val isTail			= Bool()
	isTail				:= Bool(false)

	
		 io.packetReady		:= queue.io.enq.ready
		 queue.io.enq.valid	:= io.packetValid
		 queue.io.enq.bits	:= io.packet

		 when( state === s_idle){
			when (queue.io.deq.valid){	state 			:= s_createHeadFlit 
									queue.io.deq.ready	:= Bool(false)
									io.flitValid	:= Bool(false)		  }
			.otherwise 			 {	state			:= s_idle
									queue.io.deq.ready	:= Bool(true)
									io.flitValid	:= Bool(false)		  }
			io.flit						:= new Flit(parms).fromBits(UInt(0))
			headBundle2Flit.io.inHead 	:= new HeadFlit(parms).fromBits(UInt(0))
			bodyBundle2Flit.io.inBody 	:= new BodyFlit(parms).fromBits(UInt(0))
		}.elsewhen( state === s_createHeadFlit)  {
			when (io.flitReady)  { 	state			:= s_sourceAddress 	  }
			.otherwise			 {	state			:= s_createHeadFlit	  }
			io.flit						:= CreateHeadFlit(queue.io.deq.bits.destAddress, queue.io.deq.bits.command(PacketFieldIndex.packetID), parms)
			io.flitValid				:= Bool(true)
			bodyBundle2Flit.io.inBody 	:= new BodyFlit(parms).fromBits(UInt(0))
			queue.io.deq.ready 			:= Bool(false)        
		
		} .elsewhen( state === s_sourceAddress)	{
			when (io.flitReady)  { 	state			:= s_destAddress 	  }
			.otherwise			 {	state			:= s_sourceAddress	  }
			io.flit						:= CreateBodyFlit(queue.io.deq.bits.sourceAddress, queue.io.deq.bits.command(PacketFieldIndex.packetID), UInt(0), Bool(false), parms)
			io.flitValid				:= Bool(true)
			headBundle2Flit.io.inHead 	:= new HeadFlit(parms).fromBits(UInt(0))
			queue.io.deq.ready			:= Bool(false)        

		} .elsewhen( state === s_destAddress)	{
			when (io.flitReady)  { 	state			:= s_length		 	  }
			.otherwise			 {	state			:= s_destAddress	  }
			io.flit						:= CreateBodyFlit(queue.io.deq.bits.destAddress, queue.io.deq.bits.command(PacketFieldIndex.packetID), UInt(1), Bool(false), parms)
			io.flitValid				:= Bool(true)
			headBundle2Flit.io.inHead 	:= new HeadFlit(parms).fromBits(UInt(0))
			queue.io.deq.ready 			:= Bool(false)        

		}.elsewhen( state === s_length)	{
			when (io.flitReady)  { 	state			:= s_pktIDcommand 	  }
			.otherwise			 {	state			:= s_length			  }
			
			val payload = 	Cat(	queue.io.deq.bits.length(PacketFieldIndex.totalLength)(log2Up(maxPacketLength),0),
									queue.io.deq.bits.length(PacketFieldIndex.payloadLength)(log2Up(maxPacketLength),0),
									queue.io.deq.bits.length(PacketFieldIndex.additionalFlags)(packetWidth - (2*log2Up(maxPacketLength)),0)
								)
			io.flit						:= CreateBodyFlit(payload , queue.io.deq.bits.command(PacketFieldIndex.packetID), UInt(2), Bool(false), parms)
			io.flitValid				:= Bool(true)
			queue.io.deq.ready 			:= Bool(false)        
			headBundle2Flit.io.inHead 	:= new HeadFlit(parms).fromBits(UInt(0))
		}.elsewhen( state === s_pktIDcommand)	{
			when (io.flitReady)  { 	state			:= s_debug		 	  }
			.otherwise			 {	state			:= s_pktIDcommand	  }
			
			val cmdPayload = Cat(	queue.io.deq.bits.command(PacketFieldIndex.packetID)(log2Up(maxPacketLength),0),
									queue.io.deq.bits.command(PacketFieldIndex.command)(log2Up(maxPacketLength),0),
									queue.io.deq.bits.command(PacketFieldIndex.commandOptions)(packetWidth - (2*log2Up(maxPacketLength)),0)
								)
			io.flit						:= CreateBodyFlit(cmdPayload , queue.io.deq.bits.command(PacketFieldIndex.packetID), UInt(3), Bool(false), parms)
			io.flitValid				:= Bool(true)
			headBundle2Flit.io.inHead 	:= new HeadFlit(parms).fromBits(UInt(0))
			queue.io.deq.ready 			:= Bool(false)        
		
		} .elsewhen( state === s_debug)	{
			val payloadLength = queue.io.deq.bits.length(PacketFieldIndex.payloadLength)
			when (io.flitReady 		&& (payloadLength > UInt(0)))  	{ 	state				:= s_payload	 	  }
			.elsewhen(io.flitReady 	&& Chisel.Bool(payloadLength == UInt(0)))	{   state				:= s_idle
																		queue.io.deq.ready	:= Bool(false)		  }
			.otherwise			 {	state			:= s_debug		}
			isTail						:= UInt(payloadLength) === UInt(0)
			io.flit						:= CreateBodyFlit(queue.io.deq.bits.debug, queue.io.deq.bits.command(PacketFieldIndex.packetID), UInt(4), isTail, parms)
			io.flitValid				:= Bool(true)
			headBundle2Flit.io.inHead 	:= new HeadFlit(parms).fromBits(UInt(0))
			queue.io.deq.ready 			:= Bool(false)        

		} .elsewhen( state === s_payload) {
			val payloadLength = queue.io.deq.bits.length(PacketFieldIndex.payloadLength)
			when (io.flitReady) {	
				when (payloadPhase < payloadLength) {
					isTail				:= UInt(payloadPhase) === (UInt(payloadLength)-UInt(1))
					printf("DEBUG:: payload Phase: %d payloadLength: %d\n", payloadPhase, payloadLength) 
					state 				:= s_payload					
					io.flit				:= CreateBodyFlit(queue.io.deq.bits.payload(payloadPhase), queue.io.deq.bits.command(PacketFieldIndex.packetID), (UInt(5) + payloadPhase), isTail, parms)
					io.flitValid		:= Bool(true)
					payloadPhase		:= payloadPhase + UInt(1)
					queue.io.deq.ready	:= Bool(false)

				}.otherwise {
					state 				:= s_idle
					io.flit				:= new Flit(parms).fromBits(UInt(0))
					io.flitValid		:= Bool(false)
					payloadPhase		:= UInt(0)
					queue.io.deq.ready	:= Bool(true)
					
				}		
			}.otherwise {
				state				:= s_payload
				payloadPhase		:= payloadPhase
				queue.io.deq.ready	:= Bool(false)
			}
			headBundle2Flit.io.inHead 	:= new HeadFlit(parms).fromBits(UInt(0))
		}.otherwise{
				state						:= s_idle
				queue.io.deq.ready			:= Bool(true)		   
				io.flitValid				:= Bool(false)		  
				io.flit						:= new Flit(parms).fromBits(UInt(0))
				headBundle2Flit.io.inHead 	:= new HeadFlit(parms).fromBits(UInt(0))
				bodyBundle2Flit.io.inBody 	:= new BodyFlit(parms).fromBits(UInt(0))
		}
	
		

	def CreateHeadFlit(destAddress : UInt, packetID : UInt, parms : Parameters) : Flit =  {
		val offset			= 16
		headBundle2Flit.io.inHead.isTail		:= Bool(false)
		headBundle2Flit.io.inHead.packetID		:= packetID
		headBundle2Flit.io.inHead.packetType	:= UInt(0)
		for (d <- 0 until Dim){
			//headBundle2Flit.io.inHead.destination(d)	:= destAddress(offset+(d*log2Up(destCordWidth))+log2Up(destCordWidth), offset+(d*log2Up(destCordWidth)))
			headBundle2Flit.io.inHead.destination(d)	:= destAddress(offset+(d*8)+log2Up(destCordWidth), offset+(d*8))
		}	
		//for (i <- 0 until C){
		//	headBundle2Flit.io.inHead.destination(i+Dim)	:= destAddress(offset+((i+Dim)*8)+log2Up(destCordWidth), offset+((i+Dim)*8))
	//	}
		headBundle2Flit.io.inHead.vcPort		:= UInt(0)
		
		val flit = headBundle2Flit.io.outFlit  
		flit
	}
	
	def CreateBodyFlit(payload : UInt, packetID : UInt, flitID : UInt, isTail : Bool, parms : Parameters) : Flit = {
		
		bodyBundle2Flit.io.inBody.isTail	:= isTail
		bodyBundle2Flit.io.inBody.packetID	:= packetID
		bodyBundle2Flit.io.inBody.vcPort	:= UInt(0)
		bodyBundle2Flit.io.inBody.flitID	:= flitID
		bodyBundle2Flit.io.inBody.payload	:= payload

		val flit = bodyBundle2Flit.io.outFlit
		flit
	}
	

}
	
	
class PacketToFlitTest(c: PacketToFlit) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	reset(1)
	poke(c.io.packetValid, 0)
	poke(c.io.flitReady, 1)
	step(1) 	
	val totalLength = 10
	val payloadLength = 5

	//setup a packet

	poke(c.io.packet.sourceAddress, 0xDEAD)
	poke(c.io.packet.destAddress, 0xBEEF)
	poke(c.io.packet.length(0), totalLength)
	poke(c.io.packet.length(PacketFieldIndex.payloadLength), payloadLength)
	poke(c.io.packet.length(PacketFieldIndex.additionalFlags), 0)
	poke(c.io.packet.command(PacketFieldIndex.command), 0xA)
	poke(c.io.packet.command(PacketFieldIndex.commandOptions), 0x5)
	poke(c.io.packet.debug, 0)
	for(phase <- 0 until payloadLength){
		poke(c.io.packet.payload(phase), 10*phase)
	}
	
	step(1)
	expect(c.io.flitValid, 0)
	expect(c.io.packetReady, 1)
	poke(c.io.packetValid, 1)
	step(1)
	expect(c.io.packetReady, 0)
	expect(c.io.flitValid, 1)
	step(9)
	expect(c.io.packetReady, 0)
	expect(c.io.flitValid, 1)
	step(1)
	expect(c.io.flitValid, 1)
	expect(c.io.packetReady, 0)
	step(1)
	expect(c.io.flitValid, 0)
	expect(c.io.packetReady, 1)
	poke(c.io.packetValid, 0)
	step(5)
	
	
	
}
