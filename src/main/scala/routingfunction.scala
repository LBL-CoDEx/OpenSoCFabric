package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

/*
abstract class RoutingFunction(parms: Parameters) extends Module(parms) {
	val widthRoutingMode = parms.get[Int]("widthRoutingMode")
	val widthPortID = parms.get[Int]("widthPortID")
	val widthVCID = parms.get[Int]("widthVCID")
	val io = new Bundle {
		// val RoutingMode = UInt(width = RoutingModeWidth)
		// val PacketType = PacketEnum // Enum of Packet Types
		// val Destination = Vec(Dim + 1)[UInt(width = AddressWidth)] // Addresswidth is the width for one's dimension coordinate. This has one more entry (the processor ID port to be ejected in the most significant position)
		// val RouterCoords = Vec(Dim)[UInt(width = AddressWidth)]
		val headFlit = new HeadFlit(parms)
		
		val inRoutingMode = UInt(width = widthRoutingMode)
		val inPortID = UInt(width = widthPortID) // Input the flit arrived in the current router. Width depends on router radix.
		val inVCID = UInt(width = widthVCID) // VC the flit arrived in the current router. Width depends on number of VCs.
		
		val outRoutingMode = UInt(width = widthRoutingMode) 
		val outPortID = UInt(width = widthPortID)
		val outVCID = UInt(width = widthVCID)
	}
	
}
*/

// This class also provides member routing functions with the proper interface
// It passes a child routing function instance to each instantiated router
abstract class RoutingFunction(parms: Parameters) extends Module(parms) {
	val routingCoord = parms.get[Vector[Int]]("routingCoord")
	val numResources = parms.get[Int]("numResources")
	val numVCs = parms.get[Int]("numVCs")
	val io = new Bundle {
		val inHeadFlit = new HeadFlit(parms).asInput
		val outHeadFlit = new HeadFlit(parms).asOutput
		val result = UInt(width=log2Up(numResources)).asOutput
		// val vcsAvailable = UInt(width=(numVCs*numResources)).asOutput
		val vcsAvailable = Vec.fill(numResources) { UInt(width=numVCs) }.asOutput
	}
}

// Dimension-order routing for the mesh topology. The destination field it expects is a vector of coordinates (least significant index is the first dimension
// such as X). In the most significant index position is a processor ID in the destination router. For example, 1 means ejection channel 1 once the flit/packet reaches the router.
class CMeshDOR(parms: Parameters) extends RoutingFunction(parms) {

	def priorityEncoder(a: UInt, b: UInt) : UInt = {
		val result = UInt(width=Math.max(a.getWidth, b.getWidth))
		when (a != UInt(0)) {
			result := a
		} .otherwise {
			result := b
		}
		result
	}

	val Dim = parms.get[Int]("TopologyDimension") // Dimension of topology
	val K = parms.get[Vector[Int]]("RoutersPerDim") // Routers per dimension.
	val C = parms.get[Int]("Concentration") // Processors (endpoints) per router.

	val flitDest = io.inHeadFlit.destination
	val dimResults = Vec.fill(Dim) {UInt(width=log2Up(numResources))}
	// Now we examine the coordinates one by one
	// CONVENTION: We assume the LSBs contain the destination in the first dimension (e.g., X), and so on.
	for (i <- 0 until Dim) {
		when (flitDest(i) > UInt(routingCoord(i))) {
			dimResults(i) := UInt(i * 2 + 0 + C) // Remember the convention on how output ports are numbered
		} .elsewhen (flitDest(i) < UInt(routingCoord(i))) {
			dimResults(i) := UInt(i * 2 + 1 + C)
		} .otherwise { // (flitDest(i) === UInt(routingCoord(i)))
			dimResults(i) := UInt(0)
		}
	}
	val resultReduction = dimResults.reduceLeft(priorityEncoder(_,_))
	when (resultReduction === UInt(0)) {
		io.result := flitDest(Dim)
	} .otherwise {
		io.result := resultReduction
	}

	// io.vcsAvailable := Fill(UInt(1,width=1), numVCs*numResources)
	io.vcsAvailable.zipWithIndex.foreach{ case (a, i) =>
		a := Fill( numVCs, io.result === UInt(i) )
	}
	
	io.outHeadFlit <> io.inHeadFlit
}

class CMDORTester (c: CMeshDOR) extends Tester(c) {
	val flitWidth = c.io.inHeadFlit.getWidth()
	val numVCs = c.io.inHeadFlit.numVCs
	val packetIDWidth = c.io.inHeadFlit.packetIDWidth
	val packetTypeWidth = c.io.inHeadFlit.packetTypeWidth
	val destCordWidth = c.io.inHeadFlit.destCordWidth
	val destCordDim = c.io.inHeadFlit.destCordDim

	val routerCoord = c.routingCoord
	
	printf("numResources: %d\n", c.numResources)
	printf("destCordWidth: %d\tdestCordDim: %d\n", destCordWidth, destCordDim)

	val numTestRuns = 10

	val randoms = Array.fill(numTestRuns) ( Array.fill(7) (BigInt(0)) )
	val results = Array.fill(numTestRuns) ( BigInt(0) )

	for (j <- 0 until numTestRuns) {
		for (i <- 0 until 7) {
			i match {
				case 6 =>
					randoms(j)(i) = BigInt(Random.nextInt(Math.pow(2, packetIDWidth).toInt))
				case 5 =>
					randoms(j)(i) = BigInt(Random.nextInt(Math.pow(2, 1).toInt))
				case 4 =>
					randoms(j)(i) = BigInt(Random.nextInt(numVCs))
				case 3 =>
					randoms(j)(i) = BigInt(Random.nextInt(Math.pow(2, packetTypeWidth).toInt))
				case 2 =>
					randoms(j)(i) = BigInt(Random.nextInt(c.C).toInt)//0) // BigInt(Random.nextInt(Math.pow(2, destCordWidth).toInt))
				case 1 =>
					randoms(j)(i) = BigInt(Random.nextInt(c.K(1)).toInt)
				case 0 =>
					randoms(j)(i) = BigInt(Random.nextInt(c.K(0)).toInt)
			}
		}
		printf("randoms(0): %d\trandoms(1): %d\n", randoms(j)(0), randoms(j)(1))
		if (randoms(j)(0) > BigInt(routerCoord(0))) {
			results(j) = 0 * 2 + 0 + c.C
		} else if (randoms(j)(0) < BigInt(routerCoord(0))) {
			results(j) = 0 * 2 + 1 + c.C
		} else if (randoms(j)(1) > BigInt(routerCoord(1))) {
			results(j) = 1 * 2 + 0 + c.C
		} else if (randoms(j)(1) < BigInt(routerCoord(1))) {
			results(j) = 1 * 2 + 1 + c.C
		} else {
			results(j) = randoms(j)(2)
		}
	}

	printf("---\n")
	for (i <- 0 until numTestRuns) {
		poke(c.io.inHeadFlit, randoms(i))
		step(1)
		expect(c.io.outHeadFlit, randoms(i))
		// peek(c.io.result)
		expect(c.io.result, results(i))
		printf("---\n")
	}
}

// Dimension-order routing for the flattened butterfly topology. The destination field it expects is a vector of coordinates (least significant index is the first dimension
// such as X). In the most significant index position is a processor ID in the destination router. For example, 1 means ejection channel 1 once the flit/packet reaches the router.
class CFlatBflyDOR(parms: Parameters) extends RoutingFunction(parms) {

	def priorityEncoder(a: UInt, b: UInt) : UInt = {
		val result = UInt(width=Math.max(a.getWidth, b.getWidth))
		when (a != UInt(0)) {
			result := a
		} .otherwise {
			result := b
		}
		result
	}

	val Dim = parms.get[Int]("TopologyDimension") // Dimension of topology
	val K = parms.get[Vector[Int]]("RoutersPerDim") // Routers per dimension.
	val C = parms.get[Int]("Concentration") // Processors (endpoints) per router.

	val flitDest = io.inHeadFlit.destination
	val dimResults = Vec.fill(Dim) {UInt(width=log2Up(numResources))}
	// Now we examine the coordinates one by one
	// CONVENTION: We assume the LSBs contain the destination in the first dimension (e.g., X), and so on.
	for (i <- 0 until Dim) {
		val dimOffset = if (i == 0) { 0 } else { K.slice(0, i).sum - i }
		when (flitDest(i) > UInt(routingCoord(i))) {
			dimResults(i) := flitDest(i) - UInt(routingCoord(i), width=log2Up(numResources)) + UInt(C - 1, width=log2Up(numResources)) + UInt(dimOffset, width=log2Up(numResources)) // Remember the convention on how output ports are numbered
		} .elsewhen (flitDest(i) < UInt(routingCoord(i))) {
			dimResults(i) := UInt(K(i) - 1, width=log2Up(numResources)) - flitDest(i) + UInt(C - 1 + dimOffset, width=log2Up(numResources))
		} .otherwise { // (flitDest(i) === UInt(routingCoord(i)))
			dimResults(i) := UInt(0)
		}
	}
	val resultReduction = dimResults.reduceLeft(priorityEncoder(_,_))
	when (resultReduction === UInt(0)) {
		io.result := flitDest(Dim)
	} .otherwise {
		io.result := resultReduction
	}
	
	// io.vcsAvailable := Fill(UInt(1,width=1), numVCs*numResources)
	io.vcsAvailable.zipWithIndex.foreach{ case (a, i) =>
		a := Fill( numVCs, io.result === UInt(i) )
	}

	io.outHeadFlit <> io.inHeadFlit
}

class CFlatBflyDORTester (c: CFlatBflyDOR) extends Tester(c) {
	val flitWidth = c.io.inHeadFlit.getWidth()
	val numVCs = c.io.inHeadFlit.numVCs
	val packetIDWidth = c.io.inHeadFlit.packetIDWidth
	val packetTypeWidth = c.io.inHeadFlit.packetTypeWidth
	val destCordWidth = c.io.inHeadFlit.destCordWidth
	val destCordDim = c.io.inHeadFlit.destCordDim

	val routerCoord = c.routingCoord
	
	printf("numResources: %d\n", c.numResources)
	printf("destCordWidth: %d\tdestCordDim: %d\n", destCordWidth, destCordDim)
	printf("routerCoord: (%d, %d)\n", routerCoord(0), routerCoord(1))
	printf("K: (%d, %d)\n", c.K(0), c.K(1))
	val numTestRuns = 100

	val randoms = Array.fill(numTestRuns) ( Array.fill(7) (BigInt(0)) )
	val results = Array.fill(numTestRuns) ( BigInt(0) )

	for (j <- 0 until numTestRuns) {
		for (i <- 0 until 7) {
			i match {
				case 6 =>
					randoms(j)(i) = BigInt(Random.nextInt(Math.pow(2, packetIDWidth).toInt))
				case 5 =>
					randoms(j)(i) = BigInt(Random.nextInt(Math.pow(2, 1).toInt))
				case 4 =>
					randoms(j)(i) = BigInt(Random.nextInt(numVCs))
				case 3 =>
					randoms(j)(i) = BigInt(Random.nextInt(Math.pow(2, packetTypeWidth).toInt))
				case 2 =>
					randoms(j)(i) = BigInt(Random.nextInt(c.C).toInt)//0) // BigInt(Random.nextInt(Math.pow(2, destCordWidth).toInt))
				case 1 =>
					randoms(j)(i) = BigInt(Random.nextInt(c.K(1)).toInt)
				case 0 =>
					randoms(j)(i) = BigInt(Random.nextInt(c.K(0)).toInt)
			}
		}
		printf("%d: randoms(0): %d\trandoms(1): %d\n", j, randoms(j)(0), randoms(j)(1))
		if (randoms(j)(0) > BigInt(routerCoord(0))) {
			results(j) = randoms(j)(0) - routerCoord(0) + (c.C - 1)
		} else if (randoms(j)(0) < BigInt(routerCoord(0))) {
			results(j) = (c.K(0) - 1) - randoms(j)(0) + (c.C - 1)
		} else if (randoms(j)(1) > BigInt(routerCoord(1))) {
			results(j) = randoms(j)(1) - routerCoord(1) + (c.C - 1) + c.K.slice(0,1).sum - 1
		} else if (randoms(j)(1) < BigInt(routerCoord(1))) {
			results(j) = (c.K(1) - 1) - randoms(j)(1) + (c.C - 1) + c.K.slice(0,1).sum - 1
		} else {
			results(j) = randoms(j)(2)
		}
	}

	printf("---\n")
	for (i <- 0 until numTestRuns) {
		poke(c.io.inHeadFlit, randoms(i))
		step(1)
		expect(c.io.outHeadFlit, randoms(i))
		// peek(c.io.result)
		expect(c.io.result, results(i))
		printf("---\n")
	}
}
