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
	val lastGrant = Chisel.Reg(init=UInt(1, width=numReqs))
	
	val requestsBitsVec = Vec.fill(io.requests.length) { UInt(width = 1) }
	requestsBitsVec.zipWithIndex.foreach{ case (e, i) => e := io.requests(i).request.toUInt() }
	val requestsBits = UInt(width = io.requests.length)
	requestsBits := requestsBitsVec.toBits
	
	val passSelectL0 = UInt(width = numReqs + 1)
	val passSelectL1 = UInt(width = numReqs)
	val winner = UInt(width = numReqs)
 	passSelectL0 := UInt(0)
 	passSelectL1 := UInt(0)
 	winner := UInt(0)

 	val lastGrantUInt = UInt(width = log2Up(numReqs))
 	lastGrantUInt := Chisel.OHToUInt(lastGrant)
 	val lockRelease = Bool()
 	lockRelease := io.requests(lastGrantUInt).releaseLock

	/* Check to make sure resource is not busy */
	when (~io.resource.ready) {
		winner := UInt(0)
	} .otherwise {
		/* Locking logic encapsulating Round Robin logic */
		when ( lastGrant(lastGrantUInt) && requestsBits(lastGrantUInt) && ~lockRelease ) {
			/* If Locked (i.e. request granted but still requesting)
				make sure to keep granting to that port */
			winner := lastGrant
		} .otherwise {
			/* Otherwise, select next requesting port */
			// passSelectL0 := Cat(Bool(false).toUInt, ~requestsBits) + Cat(Bool(false).toUInt, Cat(lastGrant(numReqs-2, 0), lastGrant(numReqs-1)))
			passSelectL0 := Cat(Bool(false).toUInt, ~requestsBits) + Cat(Bool(false).toUInt, lastGrant(numReqs-2, 0), lastGrant(numReqs-1))
			passSelectL1 := ~requestsBits + Bool(true).toUInt
			winner := Mux(passSelectL0(numReqs), passSelectL1, passSelectL0(numReqs-1, 0)) & requestsBits
			lastGrant := Mux(orR(winner), winner, lastGrant)
		}
	}
	
	(0 until numReqs).map(i => io.requests(i).grant := winner(i).toBool())
	io.chosen := Chisel.OHToUInt(winner)
	io.resource.valid := io.requests(io.chosen).request & (winner != UInt(0))
}

class RRArbiterTest(c: RRArbiter) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	def noting(i: Int) : Int = if (i == 1) 0 else 1

	val numPorts : Int = c.numReqs

	// val inputArray = Array(Integer.parseInt("01001001", 2),
	// 	Integer.parseInt("01001001", 2),
	// 	Integer.parseInt("01001001", 2),
	// 	Integer.parseInt("00001001", 2),
	// 	Integer.parseInt("11001000", 2),
	// 	Integer.parseInt("11001001", 2),
	// 	Integer.parseInt("01001001", 2),
	// 	Integer.parseInt("01001001", 2))
	val inputArray = Array(Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2))
	val outputArray = Array(Integer.parseInt("00000000", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("01000000", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("01000000", 2),
		Integer.parseInt("00000001", 2))
	val lockArray = Array(Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2))
	val chosenPort = Array(0, 3, 6, 0, 3, 3, 6, 0)
	val resourceReady = Array(false, true, true, true, true, true, true, true)
	val cyclesToRun = 8
	
	step(1)
	
	for (cycle <- 0 until cyclesToRun) {
		poke(c.io.resource.ready, resourceReady(cycle))
		println("inputArray(cycle): " + inputArray(cycle).toBinaryString)
		for ( i <- 0 until numPorts) {
			// var j = numPorts - 1 - i
			// poke(c.io.requests(j).request, (inputArray(cycle) & (1 << i)) >> i)
			// poke(c.io.requests(j).releaseLock, noting((lockArray(cycle) & (1 << i)) >> i))
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
	step(5)
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
/*
class RRArbiterTest(c: RRArbiter) extends Tester(c, Array(c.io)) {
	defTests {
		var allGood = true
		val numPorts : Int = c.numReqs
		val vars = new HashMap[Node, Node]()
		for (cycle <- 0 until 6) {
			var chosenPort = Random.nextInt(numPorts)
			var resourceReady = true
			if (Random.nextInt(2) == 1) {
				resourceReady = false
			}
			vars(c.io.resource.ready) = Bool(resourceReady)
			for ( i <- 0 until numPorts ) {
				vars(c.io.requests(i).request) = Bool(chosenPort == i)
			}

			if (resourceReady) {
				vars(c.io.chosen) = UInt(chosenPort)
			} else {
				vars(c.io.chosen) = UInt(0)
			}
			vars(c.io.resource.valid) = Bool(resourceReady)

			for ( i <- 0 until numPorts ) {
				vars(c.io.requests(i).grant) = Bool((chosenPort == i) && resourceReady)
			}
			allGood &= step(vars)
		}
		allGood
	}
}
*/

