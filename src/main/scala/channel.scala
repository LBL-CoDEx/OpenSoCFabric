package OpenSoC

import Chisel._

abstract class FlitCommon(val parms: Parameters) extends Bundle {
	// val flitWidth = parms.get[Int]("flitWidth")
	val packetIDWidth = parms.get[Int]("packetIDWidth")
	val numVCs = parms.get[Int]("numVCs")

	val packetID = UInt(width = packetIDWidth)
	// val isHead = Bool(false)  // Taken care of with BitUnion
	val isTail = Bool()
	
	// println("numVCs: " + numVCs)

	// if (numVCs != 0) {
		val vcPort = UInt(width = log2Up(numVCs))
	// }
	
	// val headerWidth = packetIDWidth + isTail.width //+ isHead.width

	// override def clone = { new FlitCommon(parms).asInstanceOf[this.type] }
	// override val width = flitWidth - 1
}

class HeadFlit(parms: Parameters) extends FlitCommon(parms) {
	val packetTypeWidth = parms.get[Int]("packetTypeWidth")
	val destCordWidth = parms.get[Int]("destCordWidth")
	val destCordDim = parms.get[Int]("destCordDim")
	
	val packetType = UInt(width = packetTypeWidth)
	val destination = Vec.fill(destCordDim){UInt(width = destCordWidth)}
	
	/*
	val RoutingMode = UInt(width = RoutingModeWidth)
	// Possible Extentions
	//	Error Detection/Correction
	*/

	override def clone = { new HeadFlit(parms).asInstanceOf[this.type] }
}

class BodyFlit(parms: Parameters) extends FlitCommon(parms) {
	val flitIDWidth = parms.get[Int]("flitIDWidth")
	val payloadWidth = parms.get[Int]("payloadWidth")
	

	val flitID = UInt(width = flitIDWidth)
	val payload = UInt(width = payloadWidth)

	override def clone = { new BodyFlit(parms).asInstanceOf[this.type] }
}

class Flit(parms: Parameters) extends Bundle {
	val union = new BitUnion(Map("Head" -> new HeadFlit(parms), "Body" -> new BodyFlit(parms)))
	val x = Chisel.UInt(width = union.width)
	val numVCs = parms.get[Int]("numVCs")

	def asHead(dummy: Int = 0) : HeadFlit = union.unpack[HeadFlit]("Head", x)
	def asBody(dummy: Int = 0) : BodyFlit = union.unpack[BodyFlit]("Body", x)

	def whenHead(block: HeadFlit => Unit) { union.whenTag[HeadFlit]("Head", x)(block) }
	def whenBody(block: BodyFlit => Unit) { union.whenTag[BodyFlit]("Body", x)(block) }

	def isHead(dummy: Int = 0) : Bool = union.tagEquals("Head", x)
	def isBody(dummy: Int = 0) : Bool = union.tagEquals("Body", x)
	def isTail(dummy: Int = 0) : Bool = {
		val tailBit = Bool()
		when (isHead()) {
			tailBit := union.unpack[HeadFlit]("Head", x).isTail
		} .otherwise {
			tailBit := union.unpack[BodyFlit]("Body", x).isTail
		}
		tailBit
	}
	def getVCPort(dummy: Int = 0) : UInt = {
		val vcBits = UInt(width = log2Up(numVCs))
		when (isHead()) {
			vcBits := union.unpack[HeadFlit]("Head", x).vcPort
		} .otherwise {
			vcBits := union.unpack[BodyFlit]("Body", x).vcPort
		}
		vcBits
	}

	override def clone = { new Flit(parms).asInstanceOf[this.type] }
	// override def width : Int = {x.width}
}

object Flit {
	def head(h: HeadFlit) : Flit = {
		val f = new Flit(h.parms)
		f.x := f.union.pack("Head", h)
		f
	}

	def body(b: BodyFlit) : Flit = {
		val f = new Flit(b.parms)
		f.x := f.union.pack("Body", b)
		f
	}

	def fromBits(n: UInt, parms: Parameters) : Flit = {
		val f = new Flit(parms)
		f.x := n
		f
	}
	
	/*
	def zeroHead(parms: Parameters) : HeadFlit = {
		val x = new HeadFlit(parms)
		x.
	}
	*/
}

class ChannelVC(parms: Parameters) extends Bundle {
	val numVCs = parms.get[Int]("numVCs")
	
	val flit = new Flit(parms).asInput
	val credit = Vec.fill(numVCs) { new Credit() } // Direction as Output in class def
}

class Channel(parms: Parameters) extends Bundle {
	val flit = new Flit(parms).asInput
	val credit = new Credit() // Direction as Output in class def
	
}

class ReplaceVCPort(parms: Parameters) extends Module(parms) {
	val numVCs = parms.get[Int]("numVCs")

	val io = new Bundle {
		val oldFlit = new Flit(parms).asInput
		val newVCPort = UInt(INPUT, log2Up(numVCs))
		val newFlit = new Flit(parms).asOutput
	}

	val h = new HeadFlit(parms)
	h.packetID := io.oldFlit.asHead().packetID
	h.isTail := io.oldFlit.asHead().isTail
	h.vcPort := io.newVCPort
	h.packetType := io.oldFlit.asHead().packetType
	h.destination.zipWithIndex.foreach{ case (e,i) => e := io.oldFlit.asHead().destination(i) }

	val b = new BodyFlit(parms)
	b.packetID := io.oldFlit.asBody().packetID
	b.isTail := io.oldFlit.asBody().isTail
	b.vcPort := io.newVCPort

	b.flitID := io.oldFlit.asBody().flitID
	b.payload := io.oldFlit.asBody().payload

	val flitVCMux = Chisel.Module( new MuxN[Flit](
		new Flit(parms), parms.child("FlitVCMux", Map(
			("n"->Soft(2))
		))
	))

	flitVCMux.io.ins(1) := Flit.head(h)
	flitVCMux.io.ins(0) := Flit.body(b)
	flitVCMux.io.sel := io.oldFlit.isHead()
	io.newFlit := flitVCMux.io.out

}
