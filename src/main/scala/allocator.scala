package OpenSoC

import Chisel._
import scala.util.Random

/*
abstract class AllocatorParams {
	val AllocatorType = UInt() // Enum
	val NumReqs = UInt()
	val NumGrants = UInt()
	val NumResources = UInt()
	val NumArbiters = UInt()
}
*/

abstract class Allocator(parms: Parameters) extends Module(parms) {
	val numReqs = parms.get[Int]("numReqs")
	val numRes = parms.get[Int]("numRes")
	val arbCtor = parms.get[Parameters=>Arbiter]("arbCtor")
	val io = new Bundle {
		val requests = Vec.fill(numRes) { Vec.fill(numReqs){ new RequestIO(parms) }.flip }
		val resources = Vec.fill(numRes) { new ResourceIO }
		val chosens = Vec.fill(numRes) { UInt(OUTPUT, Chisel.log2Up(numReqs)) }
	}
}

class SwitchAllocator(parms: Parameters) extends Allocator(parms) {	
	for (i <- 0 until numRes) {
		val arb = Chisel.Module ( arbCtor(parms.child(("SWArb", i), Map(
			("numReqs"->Soft(numReqs))
		))) )
		arb.io.requests <> io.requests(i)
		io.resources(i) <> arb.io.resource
		io.chosens(i) <> arb.io.chosen
	}
}

class SwitchAllocTest(c: Allocator) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0
	
	val numReqs = c.numReqs
	val numRes = c.numRes

	val numRuns = 1

	printf("---\n")
	for (i <- 0 until numRes) {
		for (j <- 0 until numReqs) {
			// peek(c.io.requests(i)(j).grant)//, BigInt(Random.nextInt(2).toInt))
			expect(c.io.requests(i)(j).grant, BigInt(0))
		}
		// peek(c.io.resources(i).valid)
		expect(c.io.resources(i).valid, BigInt(0))
		// peek(c.io.chosens(i))
		expect(c.io.chosens(i), BigInt(0))
	}

	for (run <- 0 until numRuns) {
		val requests = Array.fill(numRes) (Array.fill(numReqs) (BigInt(Random.nextInt(2).toInt)))
		// val requests = Array.fill(numRes) (Array.fill(numReqs) (BigInt(0)))
		val chosens = Array.fill(numRes) (BigInt(-1))

		for (i <- 0 until numRes) {
			for (j <- 0 until numReqs) {
				printf("i: %d\tj: %d\t", i, j)
				printf("chosens(%d)): %d\trequests(%d)(%d): %d\n", i, chosens(i), i, j, requests(i)(j))
				if ((chosens(i) == -1) && (requests(i)(j) == 1)) {
					chosens(i) = BigInt(j)
				}
			}
			printf("chosens(%d): %d\n", i, chosens(i))
		}

		printf("---\n")
		for (i <- 0 until numRes) {
			for (j <- 0 until numReqs) {
				poke(c.io.requests(i)(j).request, requests(i)(j))
				poke(c.io.requests(i)(j).releaseLock, 0)
			}
			poke(c.io.resources(i).ready, BigInt(1))
		}
		step(1)
		printf("---\n")
		for (i <- 0 until numRes) {
			for (j <- 0 until numReqs) {
				// peek(c.io.requests(i)(j).grant)//, BigInt(Random.nextInt(2).toInt))
				expect(c.io.requests(i)(j).grant, j == chosens(i))
				poke(c.io.requests(i)(j).releaseLock, 1)
			}
			if (chosens(i) != BigInt(-1)) {
				// peek(c.io.resources(i).valid)
				expect(c.io.resources(i).valid, BigInt(1))
				// peek(c.io.chosens(i))
				expect(c.io.chosens(i), chosens(i))
			} else {
				expect(c.io.resources(i).valid, BigInt(0))
				expect(c.io.chosens(i), BigInt(0))
			}
		}
		step(1)
	}
}

class VCAllocator(parms: Parameters) extends Allocator(parms) {
	val numVCs = parms.get[Int]("numVCs")
	for (i <- 0 until numRes) {
		val arb = Chisel.Module ( arbCtor(parms.child(("SWArb", i), Map(
			("numReqs"->Soft(numReqs))
		))) )
		arb.io.requests <> io.requests(i)
		io.resources(i) <> arb.io.resource
		io.chosens(i) <> arb.io.chosen
	}

	// FARZAD: In my opinion, based on things discussed, we need two sets allocators here
	// One allocator will assign an input to an output. This output is actually a set of VCs.
	// This second allocator will select which VC the input will be assigned to. The best way I can think of at the moment is to have an inverted allocator, one where the requests come from VCs and the resourses are inputs.
}

