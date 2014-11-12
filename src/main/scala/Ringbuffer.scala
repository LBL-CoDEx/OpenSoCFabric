package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

class RingBuffer(parms: Parameters) extends Module(parms) {

	val bufferWidth			= parms.get[Int]("widthRingBuffer")
	val pointerCount		= parms.get[Int]("pointerCount")
	val totalBufferEntries	= parms.get[Int]("totalRingBufferEntries")

	//Ring Buffer interface:
	//	- read and write enables are per pointer.  
	//	- when enable is asserted, it is assumed valid data exisits in the buffer
	//	- push and pushData are used to add data to the end of the ring
	//		if the buffer is empty, or at t=0, push will write to the entry pointed to
	//		by read / write pointer 0. 
	// 	- all enables (including push) are assumed to be single cycle pulses
	// 	- if you assert pop and an enable on the same cycle... WTF happens?
	val io = new Bundle {
			val writeEnable			= Vec.fill(pointerCount){ Bool( INPUT  ) }
			val readDataValid		= Vec.fill(pointerCount){ Bool( OUTPUT ) }
			val pop					= Bool( INPUT )	//Advances all read and write pointers
			val readPointerData		= Vec.fill(pointerCount){ UInt( OUTPUT, width = bufferWidth ) }
			val writePointerData	= Vec.fill(pointerCount){ UInt( INPUT , width = bufferWidth ) }
			val push				= Bool( INPUT )
			val pushData			= UInt( INPUT, width = bufferWidth)
			val pushReady			= Bool( OUTPUT )
	}
	
	val buffer 			= Vec.fill(totalBufferEntries)	{ Reg(init = UInt(0, width = bufferWidth)) }
	val bufferValids	= Vec.fill(totalBufferEntries)  { Reg(init = UInt(0, width = 1)) }
	val accessPointers	= Vec.fill(pointerCount)		{ Reg(init = UInt(0, width = log2Up(totalBufferEntries)) ) }
	val pushPointer		= Reg(init = UInt(0, width = log2Up(totalBufferEntries)) )	

	val sel  	 		= UInt(accessPointers(0)) != UInt(0)
	val bufferEmpty 	= ~orR(bufferValids.toBits)
	val pushReady		= ~andR(bufferValids.toBits)

	io.pushReady 		:= pushReady
	

	when(io.push && pushReady){
		buffer(pushPointer) 		:= io.pushData
		bufferValids(pushPointer)	:= UInt(1)
		when(pushPointer === UInt(totalBufferEntries-1)){
			pushPointer 			:= UInt(0)
		}.otherwise{
			pushPointer	   			:= (pushPointer + UInt(1)) 
		}
	}

	for(ptr <- 1 until pointerCount){
		accessPointers(ptr) := accessPointers(UInt(ptr) - UInt(1)) + UInt(1)
	}
	
	
	for (ptr <- 0 until pointerCount) {
		val readPointerDataReg      = Reg(init = UInt(0, width = bufferWidth))
		val readDataValidReg   	    = Reg(init = UInt(0, width = 1))
		readPointerDataReg := buffer(accessPointers(ptr))
		readDataValidReg   := bufferValids(accessPointers(ptr))
		when(io.writeEnable(ptr)) {
			buffer(accessPointers(ptr)) 		:= io.writePointerData(ptr)
			bufferValids(accessPointers(ptr)) 	:= UInt(1)
		}
//		when(io.pop) {
//			accessPointers(ptr) := accessPointers(ptr) + UInt(1)
//		}
		io.readDataValid(ptr)   := readDataValidReg
		io.readPointerData(ptr) := readPointerDataReg 
	}

	when(io.pop){
		bufferValids(accessPointers(0)) 	:= UInt(0)	
		for(ptr <- 0 until pointerCount){
			accessPointers(ptr) := accessPointers(ptr) + UInt(1)
		}
	}


}

	
class RingBufferTest(c: RingBuffer) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	val bufferWidth		= c.bufferWidth 
	val pointerCount	= c.pointerCount
	val totalBufferEntries	= c.totalBufferEntries 
	printf("Blah %d\n", pointerCount)

	for (i <- 0 until pointerCount) {
		poke(c.io.writeEnable(i), 0)
		poke(c.io.writePointerData(i), 0)
	}
	poke(c.io.pushData, 0)
	poke(c.io.push, 0)
	poke(c.io.pop, 0)
	expect(c.io.pushReady, 1)
	step(1)
	for (i <- 0 until (totalBufferEntries)) {
		poke(c.io.push, 1)
		poke(c.io.pushData, (i + 3))
		expect(c.io.pushReady,1)
		step(1)
	}
		poke(c.io.push, 1)
		poke(c.io.pushData, 0xBEEF)
		expect(c.io.pushReady,0)
	step(1)

	expect(c.io.pushReady,0)
	poke(c.io.pop, 1)
	poke(c.io.push, 0)
	step(1)
	expect(c.io.pushReady,1)
	
	for (i <- 0 until totalBufferEntries-1) {
		expect(c.io.readPointerData(0), (i+3))
		expect(c.io.readDataValid(0), 1)
		expect(c.io.readPointerData(1), (i+4))
		expect(c.io.readDataValid(1), 1)
		step(1)
	}
		expect(c.io.readPointerData(0), (totalBufferEntries+2))
		expect(c.io.readDataValid(0), 1)
		expect(c.io.readDataValid(1), 0)
		step(1)
		expect(c.io.readDataValid(0), 0)
		expect(c.io.readDataValid(1), 0)
		step(1)


		
	printf("---------------\n")

}
