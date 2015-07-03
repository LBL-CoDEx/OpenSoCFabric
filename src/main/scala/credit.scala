package OpenSoC

import Chisel._
import scala.util.Random

class Credit extends Bundle {
	val grant = Bool(OUTPUT)
	// val ready = Bool(INPUT)
	// val isHead = Bool(OUTPUT)
	// val isTail = Bool(OUTPUT)
	/*
	val CreditType = UInt(width = 2)	// Should be an Enum
	val VC = UInt(width = VCWidth)
	val Count = UInt(width = CountWidth)
	// Possible Extentions
	//	Error Detection/Correction
	*/
}

class CreditGen(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val outCredit = new Credit()
		val inGrant = Bool(INPUT)
		// val outReady = Bool(OUTPUT)
		// val inIsTail = Bool(INPUT)
	}
	
	io.outCredit.grant := io.inGrant
	// io.outReady := io.outCredit.ready
	// io.outCredit.isTail := io.inIsTail
}

class CreditCon(parms: Parameters) extends Module(parms) {
	val numCreds = parms.get[Int]("numCreds")
	val threshold = parms.get[Int]("credThreshold")
	val io = new Bundle {
		val inCredit = new Credit().flip()
		val inConsume = Bool(INPUT)
		val outCredit = Bool(OUTPUT)
		val almostOut = Bool(OUTPUT)

		 //val credCount = UInt(width = log2Up(numCreds)+1).asOutput
	}
	val credCount = Reg(init=UInt(numCreds, log2Up(numCreds)+1))
	
	when (credCount === UInt(numCreds)) {
		credCount := credCount - io.inConsume.toUInt()
	} .elsewhen ((credCount > UInt(threshold))) {// && (credCount < UInt(numCreds))) {
		credCount := credCount + io.inCredit.grant.toUInt() - io.inConsume.toUInt()
	} .otherwise {
		credCount := credCount + io.inCredit.grant.toUInt()
	}

    assert((UInt(credCount) <= UInt(numCreds)), "CreditCon: Exceeded max credits")

	io.outCredit := (credCount > UInt(threshold))
	io.almostOut := (credCount === UInt(threshold+1))
	// io.inCredit.ready := io.inValid
}

class CreditTester(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val inGrant = Bool(INPUT)
		// val outReady = Bool(OUTPUT)
		// val inIsTail = Bool(INPUT)

		val inConsume = Bool(INPUT)
		val outCredit = Bool(OUTPUT)
		// val outIsTail = Bool(OUTPUT)
	}
	val numCreds = parms.get[Int]("numCreds")
	
	val creditGen = Chisel.Module ( new CreditGen( parms.child("MyGen")) )
	val creditCon = Chisel.Module ( new CreditCon( parms.child("MyCon", Map(
		("numCreds"->Soft(numCreds))))) )

	creditCon.io.inCredit <> creditGen.io.outCredit
	
	creditGen.io.inGrant <> io.inGrant
	// creditGen.io.inIsTail <> io.inIsTail
	// io.outReady <> creditGen.io.outReady

	creditCon.io.inConsume <> io.inConsume
	io.outCredit <> creditCon.io.outCredit
	// io.outIsTail <> creditCon.io.outIsTail
}


class WHCreditTest(c: CreditTester) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	val randomSeed = new Random()
	peek(c.creditCon.credCount)
	poke(c.io.inConsume, 0)
	expect(c.io.outCredit, 1)
	step(1)
	printf("---\n")

	for (i <- 0 until c.numCreds) {
		// printf("->\t")
		peek(c.creditCon.credCount)
		var isTail = randomSeed.nextBoolean()
		poke(c.io.inGrant, 0)
		// poke(c.io.inIsTail, isTail)
		poke(c.io.inConsume, 1)
		step(1)
		// printf("->\t")
		peek(c.creditCon.credCount)
		expect(c.io.outCredit, i != c.numCreds - 1)
		// expect(c.io.outIsTail, isTail)
		// peek(c.io.outReady)
		printf("---\n")
	}
	step(1) // Work around for Chisel's clock_hi/clock_lo issue
	peek(c.creditCon.credCount)
	expect(c.io.outCredit, 0)
	printf("---\n")
	step(1)
	for (i <- 0 until (c.numCreds + 2)) {
		poke(c.io.inConsume, (i == 0) || (i == 3))
		poke(c.io.inGrant, 1)
		peek(c.creditCon.credCount)
		step(1)
		peek(c.creditCon.credCount)
		expect(c.io.outCredit, (1))
		// peek(c.io.outReady)
		printf("---\n")
	}
	step(1)
	expect(c.io.outCredit, 1)
}
