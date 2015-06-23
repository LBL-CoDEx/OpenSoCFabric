package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

class PacketChannel(parms: Parameters) extends Bundle{
	val packet = new Packet(parms).asInput
	val packetValid  = Bool(INPUT)
	val packetReady  = Bool(OUTPUT)
}

class PacketInjectionQ[T <: Data](parms: Parameters, tGen : Parameters => T) extends Module(parms) {

	val flitizer = parms.get[Parameters=>InputToFlit[T]]("InputFlitizer")

	val io = new Bundle {
		val in = new ReadyValid[T](parms, tGen)
		val out = new Channel(parms).flip()
	}
	val queueDepth = parms.get[Int]("queueDepth")

	val creditGen = Chisel.Module ( new CreditGen( parms.child("MyGen")) )
	val creditCon = Chisel.Module ( new CreditCon( parms.child("MyCon", Map(
		("numCreds"->Soft(queueDepth))))) )

	val queue = Chisel.Module( new Chisel.Queue(new Flit(parms), queueDepth) )
	//val packet2Flit = Chisel.Module( new PacketToFlit(parms))
	val packet2Flit = Chisel.Module( flitizer(parms))
	
	queue.io.enq.valid 				:= packet2Flit.io.flitValid
	io.in.packetReady 				:= packet2Flit.io.packetReady
	creditGen.io.outCredit		 	:= packet2Flit.io.flitValid
	creditGen.io.inGrant 			:= queue.io.deq.ready

	packet2Flit.io.packet 			:= io.in.packet
	packet2Flit.io.packetValid 		:= io.in.packetValid	
	packet2Flit.io.flitReady 		:= creditGen.io.outCredit.grant && queue.io.enq.ready
	
	queue.io.enq.bits := packet2Flit.io.flit
	
	creditCon.io.inCredit <> io.out.credit
	creditCon.io.inConsume := queue.io.deq.valid 
	queue.io.deq.ready := creditCon.io.outCredit

	io.out.flit := queue.io.deq.bits
	
}

class PacketInjectionQTest(c: PacketInjectionQ[Packet]) extends Tester(c) {

	reset(1)

	poke(c.io.in.packetValid, 0)
	step(1)
	
	val totalLength = 10
	val payloadLength = 5

	//setup a packet

	poke(c.io.in.packet.sourceAddress, 0xDEAD)
	poke(c.io.in.packet.destAddress, 0xBEEF)
	poke(c.io.in.packet.length(0), totalLength)
	poke(c.io.in.packet.length(PacketFieldIndex.payloadLength), payloadLength)
	poke(c.io.in.packet.length(PacketFieldIndex.additionalFlags), 0)
	poke(c.io.in.packet.command(PacketFieldIndex.command), 0xA)
	poke(c.io.in.packet.command(PacketFieldIndex.commandOptions), 0x5)
	poke(c.io.in.packet.debug, 0)
	for(phase <- 0 until payloadLength){
		poke(c.io.in.packet.payload(phase), 10*phase)
	}
	
	
	step(1)
	poke(c.io.in.packetValid, 1)
	step(1)
	poke(c.io.in.packetValid, 0)
	expect(c.io.out.flitValid, 1)
	
	step(1)
	expect(c.io.out.flitValid, 1)
	
	step(1)
	expect(c.io.out.flitValid, 1)
	
	step(1)
	expect(c.io.out.flitValid, 1)
	
	step(1)
	expect(c.io.out.flitValid, 1)
	
	step(1)
	expect(c.io.out.flitValid, 1)
	
	step(2)
	expect(c.io.out.flitValid, 1)
	expect(c.io.in.packetReady, 1)

	step(1)
	expect(c.io.out.flitValid, 1)
	expect(c.io.in.packetReady, 0)
	step(3)
	expect(c.io.out.flitValid, 0)
	expect(c.io.in.packetReady, 0)
	poke(c.io.in.packetValid, 0)
	step(1)
	expect(c.io.out.flitValid, 0)
	expect(c.io.in.packetReady, 1)
	step(10)
	
}


