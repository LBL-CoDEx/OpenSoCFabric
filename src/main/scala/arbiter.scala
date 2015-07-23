package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

class RequestIO(val parms:Parameters) extends Bundle {
	val numPriorityLevels = parms.get[Int]("numPriorityLevels")
	val releaseLock = Bool(OUTPUT)
	val grant = Bool(INPUT)
	val request = Bool(OUTPUT)
	val priorityLevel = UInt(OUTPUT, width=log2Up(numPriorityLevels))
	override def clone = { new RequestIO(parms).asInstanceOf[this.type] }
}

/*
class RequestIO extends Chisel.Bundle {
	val releaseLock = Bool(OUTPUT)
	val grant = Bool(INPUT)
	val request = Bool(OUTPUT)
	val prioritylevel = UInt(OUTPUT, width=3)
}
*/

class ResourceIO extends Chisel.Bundle {
	val ready = Bool(INPUT)
	val valid = Bool(OUTPUT)

}

abstract class Arbiter(parms: Parameters) extends Module(parms) {
	val numReqs = parms.get[Int]("numReqs")
	val io = new Bundle {
		val requests = Vec.fill(numReqs){ new RequestIO(parms) }.flip
		val resource = new ResourceIO
		val chosen = UInt(OUTPUT, Chisel.log2Up(numReqs))
		}
}

// //////////////////Algorithm for Arbiter without priority ////////////////////////

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



// //////////////////Algorithm for Arbiter with priority ////////////////////////

class RRArbiterPriority(parms: Parameters) extends Arbiter(parms) {

	val numPriorityLevels = parms.get[Int]("numPriorityLevels")
	val nextGrant = Chisel.Reg(init=UInt( (1 << (numReqs-1)) , width=numReqs))
//	val nextGrant = UInt(width=numReqs)
//	nextGrant := UInt(1<<(numReqs-1))
	
	val winGrant = UInt(width=numReqs)
	winGrant := UInt(1<<(numReqs-1))
	
//	val requestsBits = Cat( (0 until numReqs).map(io.requests(_).request.toUInt() ))
	val requestsBits = Cat( (0 until numReqs).map(io.requests(_).request.toUInt() ).reverse )
	
	//val PArraySorted = Vec.fill(numReqs){Reg(init=UInt(0,width=numReqs))}
	val PArraySorted = Vec.fill(numPriorityLevels){Vec.fill(numReqs){Reg(init=UInt(0,width=1))}}

	
	val passSelectL0 = UInt(width = numReqs + 1)
	val passSelectL1 = UInt(width = numReqs)
//	val passSelectL0 = Chisel.Reg(init=UInt((0), width = numReqs+1))
//	val passSelectL1 = Chisel.Reg(init=UInt((0), width = numReqs))


//	val winner = UInt(width = numReqs)
	
	val winner       = UInt(width=numReqs)
//	val winner = Chisel.Reg(init=UInt((0), width = numReqs))
//	val pmax = Chisel.Reg(init = UInt((numReqs-1) , width = log2Up(numReqs)))
	val pmax         = UInt(width = log2Up(numReqs))
//	val winnerPort = Chisel.Reg(init = UInt((0), width = log2Up(numReqs)))
//	val winnerPort   = UInt(width = log2Up(numReqs))
//	val maxi = Chisel.Reg(init = UInt((0),width = log2Up(numReqs)))
//	val maxi         = UInt(width = log2Up(numReqs))
//	val maxj = Chisel.Reg(init = UInt((0),width = log2Up(numReqs)))
//	val maxj         = UInt(width = log2Up(numReqs))
//	val tempi = Chisel.Reg(init = UInt((0),width = log2Up(numReqs)))
//	val tempj = Chisel.Reg(init = UInt((0),width = log2Up(numReqs)))
 	
 	passSelectL0 := UInt(0)
	passSelectL1 := UInt(0)
 	
 	winner := UInt(0)
// 	maxi := UInt(0)
// 	maxj := UInt(0)
//      winnerPort := UInt(0)
	pmax := UInt(0)
 	val nextGrantUInt = UInt(width = log2Up(numReqs))
 	nextGrantUInt := Chisel.OHToUInt(nextGrant)
 	// pmax := Chisel.OHToUInt(nextGrant)
 	val lockRelease = Bool()
 	lockRelease := io.requests(nextGrantUInt).releaseLock
//	pmaxReg := pmax

  //  when(~io.resource.ready && ~lockRelease){
  //      winner := nextGrant
  //  }.otherwise {
  
	when (orR(requestsBits)) {
    	/* Locking logic encapsulating Round Robin logic */
    	when ( nextGrant(nextGrantUInt) && requestsBits(nextGrantUInt) && ~lockRelease ) {
    		/* If Locked (i.e. request granted but still requesting)
    			make sure to keep granting to that port */
		winGrant  := nextGrant
		nextGrant := winGrant
    	} 
    	.otherwise {

		//To refresh the Sorted Array
		for (i <- 0 until numPriorityLevels){
			PArraySorted(i) := UInt(0)
		}

		//To store the given priorities and the requesting ports in a sorted array
    	        for (i <- 0 until numReqs) {
			for (j <- 0 until numPriorityLevels) {
				when (io.requests(i).priorityLevel === UInt(numPriorityLevels-1-j)) {
					when (requestsBits(i)){
					PArraySorted(numPriorityLevels-1-j)(i) := UInt(1)
					//PArraySorted(numReqs-1-j) := PArraySorted(numReqs-1-j)+(UInt(1)<<UInt(i))
					}
				}
			}
		}

		//To find which is the max priority level available
		for (i <- 0 until numPriorityLevels) {
			for (j <- 0 until numReqs){
				when(PArraySorted(i)(j) === UInt(1)) {
					pmax := UInt(i)
				}
			}
		}

		//To find the port number having this max priority
    		passSelectL0 := Cat(Bool(false).toUInt, (~PArraySorted(pmax).toBits)) + Cat(Bool(false).toUInt, nextGrant(numReqs-2, 0), nextGrant(numReqs-1))
    		passSelectL1 := (~PArraySorted(pmax).toBits) + Bool(true).toUInt
    		winner := Mux(passSelectL0(numReqs), passSelectL1, passSelectL0(numReqs-1, 0)) & (PArraySorted(pmax).toBits)
		
		winGrant  := Mux(orR(winner), winner, nextGrant)
		nextGrant := winGrant
    	}
    	}
	(0 until numReqs).map(i => io.requests(i).grant := winGrant(i).toBool() && io.resource.ready) 
	io.chosen := Chisel.OHToUInt(winGrant) 
	io.resource.valid := io.requests(io.chosen).grant && io.resource.ready //(winner != UInt(0))
	
}
/*   //////////////////////////////////////////////////////////////////////////////////////////


		To find the port number having this max priority level
		for (i <- 1 until numReqs+1) {
			for (j <- 0 until i) {
				when (PArraySorted(pmax)(i-1) === PArraySorted(pmax)(j)) {
					tempi := PArraySorted(pmax)(i-1)
					tempj := PArraySorted(pmax)(j)
					maxi := UInt(i-1)
					maxj := UInt(j)
    	                   		when (((maxi>nextGrantUInt)&&(maxj>nextGrantUInt))||((maxi<nextGrantUInt)&&(maxj<nextGrantUInt))){
    	                     			winnerPort := Mux(maxi>maxj,maxj,maxi)
    	                   		}  
    	                   		.elsewhen ((maxi>nextGrantUInt)&&(maxj<nextGrantUInt)) {
    	                     			winnerPort := maxi
    	                   		}
    	                   		.elsewhen ((maxi<nextGrantUInt)&&(maxj>nextGrantUInt)) {
    	                     			winnerPort := maxj
    	                   		}
    	                   		.otherwise {
    	                     			when (maxi === nextGrantUInt) { winnerPort := maxj }
    	                     			.otherwise { winnerPort := maxi }
    	                   		}
				}
			}
		}
		
	        winner := (UInt(1) << winnerPort)
    	
		
//////////////////////////////////////////////////////////////////////////////////////////

    	         To resolve different inputs having same priority level
    	        for (i <- 1 until numReqs+1){
    	            for (j <- 0 until i){
    	                when (io.requests(i-1).prioritylevel === io.requests(j).prioritylevel) {
    	                  when (io.requests(j).prioritylevel >= io.requests(pmax).prioritylevel) {   
    	                   maxi := UInt(i-1)
    	                   maxj := UInt(j)
    	                   when (((maxi>nextGrantUInt)&&(maxj>nextGrantUInt))||((maxi<nextGrantUInt)&&(maxj<nextGrantUInt))){
    	                     pmax := Mux(maxi>maxj,maxj,maxi)
    	                   }  
    	                   .elsewhen ((maxi>nextGrantUInt)&&(maxj<nextGrantUInt)) {
    	                     pmax := maxi
    	                   }
    	                   .elsewhen ((maxi<nextGrantUInt)&&(maxj>nextGrantUInt)) {
    	                     pmax := maxj
    	                   }
    	                   .otherwise {
    	                     when (maxi === nextGrantUInt) { pmax := maxj }
    	                     .otherwise { pmax := maxi }
    	                   }
    	                  }
		        }
    	            }
    	        }
    	        
		
    	         To resolve different inputs having different priority levels 
    	        for (i <- 0 until numReqs){
    	            when (io.requests(i).prioritylevel > io.requests(pmax).prioritylevel) {
    	            pmax := UInt(i)
    	            }
    	        }
    	        
    	        
	       winner := UInt(UInt(1) << winnerPort)
    	        
    	        
    		Otherwise, select next requesting port 
    		passSelectL0 := Cat(Bool(false).toUInt, ~requestsBits) + Cat(Bool(false).toUInt, nextGrant(numReqs-2, 0), nextGrant(numReqs-1))
    		passSelectL1 := ~requestsBits + Bool(true).toUInt
    		winner := Mux(passSelectL0(numReqs), passSelectL1, passSelectL0(numReqs-1, 0)) & requestsBits
    		
    		
    		nextGrant := Mux(orR(winner), winner, nextGrant)
    	}
    	}
    }



////////////////////////////////////////////////////////////////////////////////////////

*/


// //////////////////Tester for Arbiter with priority ////////////////////////



class RRArbiterPriorityTest(c: RRArbiterPriority) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	def noting(i: Int) : Int = if (i == 1) 0 else 1

	val numPorts : Int = c.numReqs
	
	val plevelArray = Array(
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0),
	        Array(0,0,0,0,0,0,0,0)
	        )	
	
/*	val plevelArray = Array(
	        Array(4,3,6,7,2,0,5,1),//3
	        Array(5,5,4,4,4,0,0,0),//0
	        Array(5,5,5,5,3,2,4,1),//1
	        Array(5,5,5,5,0,0,0,4),//2
	        Array(5,5,5,5,0,0,0,7),//7
	        Array(2,4,3,0,1,0,5,6),//7
	        Array(7,5,5,4,3,0,0,7),//0
	        Array(5,7,7,7,7,7,7,3),//1
	        Array(1,0,0,0,2,0,6,6),//6
	        Array(2,2,2,2,2,2,2,2),//7
	        Array(6,6,6,6,6,6,6,6),//0
	        Array(6,6,6,4,4,6,0,0),//1
	        Array(2,3,3,4,4,0,1,1),//3
	        Array(3,2,7,4,1,5,0,7),//7
	        Array(0,0,3,2,3,3,0,1),//2
	        Array(0,0,0,0,0,0,0,0),//3
	        Array(0,0,0,0,0,0,0,0),//4
	        Array(6,6,3,5,4,2,5,2)//0
	        )	
*/

	val inputArray = Array(
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000011", 2),
		Integer.parseInt("00000011", 2),
		Integer.parseInt("00000011", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000011", 2),
		Integer.parseInt("00000011", 2),
		Integer.parseInt("00000011", 2),
		Integer.parseInt("00000011", 2),
		Integer.parseInt("00000011", 2),
		Integer.parseInt("00000011", 2),
		Integer.parseInt("00000011", 2)
	)
	
/*	val inputArray = Array(
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2)
	)
*/
/*
	val lockArray = Array(
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2),
		Integer.parseInt("11111111", 2)
	)
*/
	val lockArray = Array(
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000000", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000010", 2)
	)

	val outputArray = Array(
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000010", 2)
		)
	
/*	val outputArray = Array(
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000100", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00010000", 2),
		Integer.parseInt("00100000", 2),
		Integer.parseInt("01000000", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000100", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00010000", 2),
		Integer.parseInt("00100000", 2),
		Integer.parseInt("01000000", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2)
		)
*/
/*	val outputArray = Array(
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00000100", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("01000000", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("00000001", 2),
		Integer.parseInt("00000010", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("10000000", 2),
		Integer.parseInt("00000100", 2),
		Integer.parseInt("00001000", 2),
		Integer.parseInt("00010000", 2),
		Integer.parseInt("00000001", 2)
		)
*/
//	val chosenPort = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
	val chosenPort = Array(0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1)
//	val chosenPort = Array(0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1)
//	val chosenPort = Array(3, 0, 1, 2, 7, 7, 0, 1, 6, 7, 0, 1, 3, 7, 2, 3, 4, 0)
	val resourceReady = Array(true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true)
	val cyclesToRun = 18
	
	step(1)
	
	for (cycle <- 0 until cyclesToRun) {
		poke(c.io.resource.ready, resourceReady(cycle))
		println("inputArray(cycle): " + inputArray(cycle).toBinaryString)
		println("lockArray(cycle): " + lockArray(cycle).toBinaryString)
		for ( i <- 0 until numPorts) {
			poke(c.io.requests(i).request, (inputArray(cycle) & (1 << i)) >> i)
			poke(c.io.requests(i).releaseLock, noting((lockArray(cycle) & (1 << i)) >> i))
			poke(c.io.requests(i).priorityLevel, plevelArray(cycle)(i))
		}
		step(1)
	//	expect(
		expect(c.io.chosen, chosenPort(cycle))
		expect(c.io.resource.valid, resourceReady(cycle) && inputArray(cycle) != 0)
		for ( i <- 0 until numPorts) {
			expect(c.io.requests(i).grant, (outputArray(cycle) & (1 << i)) >> i)
		}
	}
	step(1)
}

// //////////////////Tester for Arbiter without priority ////////////////////////

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
