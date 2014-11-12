package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

class RouterRegFile(parms: Parameters) extends Module(parms) {
	val regWidth = parms.get[Int]("widthRegFile")
	val regDepth = parms.get[Int]("depthRegFile")
	val pipelineDepth = parms.get[Int]("pipelineDepth")
	val io = new Bundle {
		val writeData = UInt(INPUT, width = regWidth)
		val writeEnable = Bool(INPUT)
		val full = Bool(OUTPUT)
		val readData = UInt(OUTPUT, width = regWidth)
		val readValid = Bool(OUTPUT)
		val readIncrement = Bool(INPUT)

		val writePipelineReg = Vec.fill(pipelineDepth) { UInt(INPUT, width = regWidth) }
		val wePipelineReg = Vec.fill(pipelineDepth) { Bool(INPUT) }
		val readPipelineReg = Vec.fill(pipelineDepth) { UInt(OUTPUT, width = regWidth) }
		val rvPipelineReg = Vec.fill(pipelineDepth) { Bool(OUTPUT) }
	}

	val regFile = Vec.fill(regDepth) { Reg(init = UInt(0, width = regWidth)) }
	val regFileValid = Vec.fill(regDepth) { Reg(init = Bool(false)) }
	
	val writePointer = Reg(init = UInt(0, width = log2Up(regDepth)) )
	val readPointer = Reg(init = UInt(0, width = log2Up(regDepth)) )

	val regPipelineRegs = (0 until pipelineDepth).map( a => Reg( init = UInt(0, width = regWidth) ) )
	val regRVPipelineRegs = (0 until pipelineDepth).map( a => Reg( Bool(false) ) )


	io.full := andR(regFileValid.toBits())
	io.readValid := (writePointer != readPointer) && orR(regFileValid.toBits())

	when (io.writeEnable && !regFileValid(writePointer)) {
		regFile(writePointer) := io.writeData
		regFileValid(writePointer) := Bool(true)

		writePointer := writePointer + UInt(1)
		when (writePointer === UInt(regDepth)) {
			writePointer := UInt(0)
		}
	}

	io.readData := regFile(readPointer)
	when (io.readIncrement) {
		regFileValid(readPointer) := Bool(false)
		readPointer := readPointer + UInt(1)
		when (readPointer === UInt(regDepth)) {
			readPointer := UInt(0)
		}

		regPipelineRegs.map( _ := Bool(false) )
		// for (i <- 0 until pipelineDepth) {
		// 	regRVPipelineRegs(i) := Bool(false)
		// }
	}

	for ( i <- 0 until pipelineDepth ) {
		regRVPipelineRegs(i) := io.wePipelineReg(i)
		regPipelineRegs(i) := io.writePipelineReg(i)
	}

	regPipelineRegs.zipWithIndex.foreach{ case (a,i) =>
		io.readPipelineReg(i) := a
	}
	regRVPipelineRegs.zipWithIndex.foreach{ case (a,i) =>
		io.rvPipelineReg(i) := a
	}

	// for (i <- 0 until pipelineDepth) {
	// 	val regPipelineReg = Reg(init = UInt(0, width = regWidth))
	// 	when (io.wePipelineReg(i)) {
	// 		regPipelineReg := io.writePipelineReg(i)
	// 	}
	// 	io.readPipelineReg(i) := regPipelineReg
	// }
}

class RouterRegFileTest(c: RouterRegFile) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0
	
	val regWidth : Int = c.regWidth
	val regDepth : Int = c.regDepth

	val nums = (1 to regDepth + 1).map(x => BigInt(Random.nextInt(Math.pow(2,c.regWidth).toInt)))

	for (i <- 0 until regDepth + 1) {
		poke(c.io.writeData, nums(i))
		poke(c.io.writeEnable, true)
		expect(c.io.full, i == regDepth)
		poke(c.io.readIncrement, false)
		expect(c.io.readData, if (i == 0) BigInt(0) else nums(0))
		poke(c.io.writePipelineReg(0), if (i == 0) BigInt(0) else nums(0))
		poke(c.io.wePipelineReg(0), true)
		expect(c.io.readPipelineReg(0), if (i < 2) BigInt(0) else nums(0))
		step(1)
	}
	step(1)
	for (i <- 0 until regDepth + 1) {
		poke(c.io.readIncrement, true)
		expect(c.io.readData, nums(i))
		poke(c.io.writeEnable, i < 2)
		expect(c.io.full, i < 1)
		poke(c.io.writePipelineReg(0), nums(i))
		poke(c.io.wePipelineReg(0), true)
		expect(c.io.readPipelineReg(0), if (i == 0) nums(0) else nums(i-1))
		step(1)
	}
}
