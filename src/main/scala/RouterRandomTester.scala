package OpenSoC

import Chisel._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import scala.util.Random

class RouterRandomTester(c: SimpleRouterTestWrapper, parms: Parameters) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0
//	implicit def int(x: Int): BigInt = x
  //	implicit def int(x: Bits): BigInt = x.litValue()

	reset(1)

	val portsToDrive = c.numInChannels

	var headFlitMap = LinkedHashMap[String, BigInt]()
	var bodyFlitMap = LinkedHashMap[String, BigInt]()
	headFlitMap = LinkedHashMap( 	
					("Dest_0" 		-> 0 ),
					("Dest_1" 		-> 0 ),
					("Dest_2"		-> 0 ),
					("packetType"	-> 0 ),
					("vcPort"		-> 0 ),
					("isTail"		-> 0 ),
					("packetID"		-> 0 ) )

	bodyFlitMap = LinkedHashMap(	
					("flitID"	-> 0 ),
					("payload"	-> 0 ),
					("vcPort"	-> 0 ),
					("isTail"	-> 0 ),
					("packetID"	-> 0 ) )
	
	poke(c.io.bodyFlitsIn(0), bodyFlitMap.values.toArray)
	step(1)
	var zeroFlit = peek(c.io.bodyFlitsOut(0))
	
	for (i <- 0 until c.numInChannels) {
		poke(c.io.inChannels(i).flit, zeroFlit)
		poke(c.io.inChannels(i).flitValid, 0)
		poke(c.io.outChannels(i).credit.grant, 1)
		
	}
	step(1)

	//--------------------------------------------------
	//Random Test - Drive flits to random destinations 

	//Create Array of random destinations 
	var validScoreboard = Array.fill( portsToDrive ){ false }
	var isHead			= Array.fill( portsToDrive ){ false }

	val iterationCount		= 30
	val packetLength		= 3
	var iterationCountPerPort 	= Array.fill(portsToDrive)(0)
	var packetIndexPerPort 		= Array.fill(portsToDrive)(0)
	val dests 			= Array.ofDim[(Int,Int)](portsToDrive, iterationCount)
	val packetIDs		= Array.ofDim[BigInt](portsToDrive, iterationCount)
	val packetIDsInt	= Array.ofDim[UInt](portsToDrive, iterationCount)
	val flits			= Array.ofDim[Array[BigInt]](portsToDrive, iterationCount, packetLength)
	var packetInjTime	= new HashMap[Int,Int]()
	var cycleCount		= 1

	var curID = 0	
	for(port <- 0 until portsToDrive){
	  for(iter <-0 until iterationCount) {
		dests(port)(iter)     = (Random.nextInt(2), Random.nextInt(2))
		packetIDs(port)(iter) = curID % 256
		packetIDsInt(port)(iter) = UInt( curID % 256)
		curID += 1
	//	packetMap(packetIDsInt(port)) = dests(port)
      }
	}

	//Set packet length to constant of 3 right now - one head, two body

	//set up all flits to be driven, store in flits() array
  for(iter <- 0 until iterationCount){
	for(port <- 0 until portsToDrive){
		printf("port: %d\n", port)
		
		//create a head and body flit for each port
		headFlitMap("Dest_0") 	= dests(port)(iter)._1
		headFlitMap("Dest_1") 	= dests(port)(iter)._2 
		headFlitMap("Dest_2") 	= 0
		headFlitMap("isTail") 	= 0
		headFlitMap("packetID") = packetIDs(port)(iter)
		bodyFlitMap("packetID") = packetIDs(port)(iter)
		bodyFlitMap("isTail") 	= 0
		poke(c.io.headFlitsIn(port), headFlitMap.values.toArray)
		poke(c.io.bodyFlitsIn(port), bodyFlitMap.values.toArray)

	    step(1)
		//get head and body flits from extractor blocks, then add to array based on port & iteration#
		var myHeadFlit = peek(c.io.headFlitsOut(port))
		var myBodyFlit = peek(c.io.bodyFlitsOut(port))
		flits(port)(iter)(0) = myHeadFlit
		flits(port)(iter)(1) = myBodyFlit

		//setup the tail flit
		bodyFlitMap("isTail") 	= 1
		poke(c.io.bodyFlitsIn(port), bodyFlitMap.values.toArray)
		step(1)

		//Add the tail flit to the array of flits
		myBodyFlit = peek(c.io.bodyFlitsOut(port))
		flits(port)(iter)(2) = myBodyFlit
	  }
	}	

	//begin driving flits on all ports
	val totalCycleCount = 800

  while((iterationCountPerPort.min < iterationCount) || (cycleCount < totalCycleCount)){
	for(port <- 0 until portsToDrive){

		//if credit valid is low, then the port cannot accept flits (i.e. out of credits!)
		var creditValid = peek(c.io.inChannels(port).credit.grant) 

		//port is ready, and this port is not yet done with all iterations
		if(((creditValid > 0) && (iterationCountPerPort(port) < iterationCount)) || (iterationCountPerPort(port) == 0) ){
			//drive flit w/ credit ready
			var myFlit = flits(port)(iterationCountPerPort(port))(packetIndexPerPort(port))
			if(packetIndexPerPort(port) == 0){
				var pID = (myFlit(0).toInt & 0x0003FC00) >> 10
				packetInjTime(pID)= cycleCount
				println("adding pid: ", pID)
			}
			poke(c.io.inChannels(port).flitValid, 1)
			poke(c.io.inChannels(port).flit, myFlit)
			
			//track where we are in this packet
			packetIndexPerPort(port)    = packetIndexPerPort(port) + 1

			//have we come to the end of the packet?
			if(packetIndexPerPort(port) == packetLength){
				packetIndexPerPort(port) = 0				
				iterationCountPerPort(port) = iterationCountPerPort(port) + 1
			}
		}else {
			//port not ready... drive all zeros and deassert credit ready
			poke(c.io.inChannels(port).flit, zeroFlit)
			poke(c.io.inChannels(port).flitValid, 0)
		}
	}
		step(1)
		cycleCount+= 1
		var min = iterationCountPerPort.min
		println("min is: ", min) 
	
	//start checking process...

	for(port <- 0 until portsToDrive){
		//check the scoreboard for valid flits from last cycle		
		if(validScoreboard(port)){
			var packetID = 0
			if(isHead(port)) {
				packetID = peek(c.io.flitsOutAsHead(port).packetID).toInt
				println("Found head flit : ", packetID)
				if (packetInjTime.contains(packetID)){
					var latency = cycleCount - packetInjTime(packetID) + 1
					println("Latency for packet ID ", packetID, " is: ", latency)
				}
			} 
		}

		//look for valid flit on output port
		var validFlit = peek(c.io.outChannels(port).flitValid) > 0
		if(validFlit){
			//determine if flit is head, then translate it as appropriate 
			var myFlit = peek(c.io.outChannels(port).flit)
			if((myFlit(0).toInt & 0x1) > 0) {
				poke(c.io.flitsIn(port), myFlit)
				isHead(port) = true
			}else{
				isHead(port) = false
			}
			//mark scoreboard so flit can be checked on the next clock
			validScoreboard(port) = true
		}else{
			validScoreboard(port) = false
		}
	}
  }//end checking 

	////we've driven all flits, so make sure all inputs are set to zero
	for(port <-0 until c.numInChannels){
		poke(c.io.inChannels(port).flit, zeroFlit)
		poke(c.io.inChannels(port).flitValid, 0)
	}


	step(5)

	//expect(c.io.ports(1).out.flit, myHeadFlit)
	//expect(c.io.ports(1).out.flit, myBodyFlit)

	//drive a flit on port 1
/*	headFlitMap("Dest_0") 	= 1
	headFlitMap("Dest_1") 	= 1
	headFlitMap("Dest_2") 	= 0
	headFlitMap("isTail") 	= 0
	headFlitMap("packetID") = 5
	bodyFlitMap("packetID") = 5
	bodyFlitMap("isTail") 	= 1
	poke(c.io.headFlitIn, headFlitMap.values.toArray)
	poke(c.io.bodyFlitIn, bodyFlitMap.values.toArray)
	step(1)
	var myHeadFlit = peek(c.io.headFlitOut)
	var myBodyFlit = peek(c.io.bodyFlitOut)
	step(1)
	for (i <- 0 until c.numPorts) {
		poke(c.io.ports(i).in.flitValid, 0)
		poke(c.io.ports(i).out.credit.grant, 1)
	}
	poke(c.io.ports(1).in.flitValid, 1)
	poke(c.io.ports(1).in.flit, myHeadFlit)
	step(1)
	poke(c.io.ports(1).in.flitValid, 1)
	poke(c.io.ports(1).in.flit, myBodyFlit)
	step(1)
	poke(c.io.ports(1).in.flit, zeroFlit)
	poke(c.io.ports(1).in.flitValid, 0)
	peek(c.io.ports(3).out.flit)
	step(1)
	peek(c.io.ports(3).out.flit)
	step(1)
	peek(c.io.ports(3).out.flit)
	step(1)
	peek(c.io.ports(3).out.flit)
	step(1)
	peek(c.io.ports(3).out.flit)
	step(1)
	peek(c.io.ports(3).out.flit)
	step(1)
	peek(c.io.ports(3).out.flit)
	step(1)
	expect(c.io.ports(3).out.flit, myHeadFlit)
	step(1)	
	expect(c.io.ports(3).out.flit, myBodyFlit)
	step(1)	*/
	
}


