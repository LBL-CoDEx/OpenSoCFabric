package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

class RequestIO extends Chisel.Bundle {
	val releaseLock = Bool(OUTPUT)
	val grant = Bool(INPUT)
	val request = Bool(OUTPUT)
}

class ResourceIO extends Chisel.Bundle {
	val ready = Bool(INPUT)
	val valid = Bool(OUTPUT)
}

abstract class Arbiter(parms: Parameters) extends Module(parms) {
	val numReqs = parms.get[Int]("numReqs")
	val io = new Bundle {
		val requests = Vec.fill(numReqs){ new RequestIO }.flip
		val resource = new ResourceIO
		val chosen = UInt(OUTPUT, Chisel.log2Up(numReqs))
	}
}

class RRArbiter(parms: Parameters) extends Arbiter(parms) {
	val nextGrant = Chisel.Reg(init=UInt( (1 << (numReqs-1)) , width=numReqs))
	
	val requestsBits = Cat( (0 until numReqs).map(io.requests(_).request.toUInt() ).reverse )

	val passSelectL0 = UInt(width = numReqs + 1)
	val passSelectL1 = UInt(width = numReqs)
	val winner = UInt(width = numReqs)
 	passSelectL0 := UInt(0)
 	passSelectL1 := UInt(0)
 	winner := UInt(0)

 	val nextGrantUInt = UInt(width = log2Up(numReqs))
 	nextGrantUInt := Chisel.OHToUInt(nextGrant)
 	val lockRelease = Bool()
 	lockRelease := io.requests(nextGrantUInt).releaseLock

  //  when(~io.resource.ready && ~lockRelease){
  //      winner := nextGrant
  //  }.otherwise {
    	/* Locking logic encapsulating Round Robin logic */
    	when ( nextGrant(nextGrantUInt) && requestsBits(nextGrantUInt) && ~lockRelease ) {
    		/* If Locked (i.e. request granted but still requesting)
    			make sure to keep granting to that port */
    		winner := nextGrant
    	} .otherwise {
    		/* Otherwise, select next requesting port */
    		passSelectL0 := Cat(Bool(false).toUInt, ~requestsBits) + Cat(Bool(false).toUInt, nextGrant(numReqs-2, 0), nextGrant(numReqs-1))
    		passSelectL1 := ~requestsBits + Bool(true).toUInt
    		winner := Mux(passSelectL0(numReqs), passSelectL1, passSelectL0(numReqs-1, 0)) & requestsBits
    		nextGrant := Mux(orR(winner), winner, nextGrant)
    	}
    //}
	
	(0 until numReqs).map(i => io.requests(i).grant := winner(i).toBool() && io.resource.ready) 
	io.chosen := Chisel.OHToUInt(winner) 
	io.resource.valid := io.requests(io.chosen).grant && io.resource.ready //(winner != UInt(0))
}

class RRArbiterTest(c: RRArbiter) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	def noting(i: Int) : Int = if (i == 1) 0 else 1

	val numPorts : Int = c.numReqs

	val inputArray = Array(
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00100001", 2),
		Integer.parseInt("00000100", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000100", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00010000", 2),
		Integer.parseInt("00100000", 2),
		Integer.parseInt("01000000", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("00000000", 2)
		)
	val lockArray = Array(
		Integer.parseInt("00000000", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111110", 2),
		Integer.parseInt("11011111", 2),
		Integer.parseInt("11111011", 2),
		Integer.parseInt("01111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111110", 2),
		Integer.parseInt("11111101", 2),
		Integer.parseInt("11111011", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11110111", 2),
		Integer.parseInt("11101111", 2),
		Integer.parseInt("11011111", 2),
		Integer.parseInt("10111111", 2),
		Integer.parseInt("01111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2)
	)
	val outputArray = Array(
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00100000", 2),
		Integer.parseInt("00000100", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000100", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00010000", 2),
		Integer.parseInt("00100000", 2),
		Integer.parseInt("01000000", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("00000000", 2)
		)
	val chosenPort = Array(0, 0, 0, 0, 5, 2, 7, 0, 0, 1, 2, 3, 3, 4, 5, 6, 7, 0)
	val resourceReady = Array(false, true, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false)
	val cyclesToRun = 18
	
	step(1)
	
	for (cycle <- 0 until cyclesToRun) {
		poke(c.io.resource.ready, resourceReady(cycle))
		println("inputArray(cycle): " + inputArray(cycle).toBinaryString)
		for ( i <- 0 until numPorts) {
			poke(c.io.requests(i).request, (inputArray(cycle) & (1 << i)) >> i)
			poke(c.io.requests(i).releaseLock, noting((lockArray(cycle) & (1 << i)) >> i))
		}
		step(1)
		expect(c.io.chosen, chosenPort(cycle))
		expect(c.io.resource.valid, resourceReady(cycle) && inputArray(cycle) != 0)
		for ( i <- 0 until numPorts) {
			expect(c.io.requests(i).grant, (outputArray(cycle) & (1 << i)) >> i)
		}
	}
	step(1)
}

class DumbRRArbiterTest(c: RRArbiter) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	val numPorts : Int = c.numReqs

	val cyclesToRun = 8

	step(1)

	for (cycle <- 0 until cyclesToRun) {
		poke(c.io.resource.ready, 1)
		for ( i <- 0 until numPorts) {
			poke(c.io.requests(i).request, 1)
			poke(c.io.requests(i).releaseLock, 1)
		}
		step(1)
		expect(c.io.chosen, if (cycle % 8 > 5) (cycle - 6) else (cycle + 2) )
		expect(c.io.resource.valid, 1)
		for ( i <- 0 until numPorts) {
			expect(c.io.requests(i).grant, (i == (cycle + 2)) || (i == (cycle - 6)))
		}
	}
	step(5)
}
