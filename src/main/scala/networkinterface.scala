package OpenSoC

import Chisel._

/*
abstract class NetworkInterfaceParams {
	val NumOutChanPerEnd = UInt()
	val NumInChanPerEnd = UInt()
}

abstract class NetworkInterface extends Module {
	val io = new Bundle {
		val AXIPort = new AXI()
		val OutChannels = Vec.fill(NumChanPerEnd) {new Channel()}
		val InChannels = Vec.fill(NumChanPerEnd) {new Channel()}
	}
}
*/

abstract class InputNetworkInterface[T <: Data](parms : Parameters, tGen : Parameters => T) extends Module(parms) {
	val io = new Bundle {
		val in = new DecoupledIO[T](tGen(parms)).flip()
		val out = new Channel(parms).flip()
	}
}

class InputPacketInterface[T <: Data](parms: Parameters, tGen : Parameters => T) extends InputNetworkInterface[T](parms, tGen) {
	
	val flitizer = parms.get[Parameters=>InputToFlit[T]]("InputFlitizer")
	val queueDepth = parms.get[Int]("queueDepth")
	
	val creditCon = Chisel.Module ( new CreditCon( parms.child("MyCon", Map(
		("numCreds"->Soft(queueDepth))))) )
	val packet2Flit = Chisel.Module( flitizer(parms) )
	
	io.in.ready 					:= packet2Flit.io.packetReady
	packet2Flit.io.packet 			:= io.in.bits
	packet2Flit.io.packetValid 		:= io.in.valid 
	
	creditCon.io.inCredit <> io.out.credit
	io.out.flit := packet2Flit.io.flit
	creditCon.io.inConsume := packet2Flit.io.flitValid
	io.out.flitValid := packet2Flit.io.flitValid
	packet2Flit.io.flitReady := creditCon.io.outCredit
}
