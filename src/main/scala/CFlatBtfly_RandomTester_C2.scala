package OpenSoC

import Chisel._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import scala.util.Random
import java.io._

class OpenSoC_CFlatBtflyTester_Random_C2(c: OpenSoC_CFlatBfly[Flit], parms: Parameters) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0
//	implicit def int(x: Int): BigInt = x
  //	implicit def int(x: Bits): BigInt = x.litValue()

	reset(1)

	var headFlitMap = LinkedHashMap[String, BigInt]()
	var bodyFlitMap = LinkedHashMap[String, BigInt]()
	headFlitMap = LinkedHashMap( 	
					("Dest_0" 		-> 0 ),
					("Dest_1" 		-> 0 ),
					("Dest_2"		-> 0 ),
					("Dest_3"		-> 0 ),
					("packetType"	-> 0 ),
					("vcPort"		-> 0 ),
					("isTail"		-> 0 ),
					("packetID"		-> 0 ) )

	bodyFlitMap = LinkedHashMap(	
					("payload"	-> 0 ),
					("flitID"	-> 0 ),
					("vcPort"	-> 0 ),
					("isTail"	-> 0 ),
					("packetID"	-> 0 ) )
	
	poke(c.io.bodyFlitsIn(0), bodyFlitMap.values.toArray)

	step(1)
	var zeroFlit = peek(c.io.bodyFlitsOut(0))
	
	for (i <- 0 until c.numPorts) {
		poke(c.io.ports(i).in.packet, zeroFlit)
		poke(c.io.ports(i).in.packetValid, 0)
		poke(c.io.ports(i).out.credit.grant, 1)
		
	}
	step(1)

	//--------------------------------------------------
	//Random Test - Drive flits to random destinations 

	//Create Array of random destinations 
	var validScoreboard = Array.fill( c.numPorts ){ false }
	var isHead			= Array.fill( c.numPorts ){ false }

	def IncrementCoord (x : Vector[Int]) : Vector[Int] = {
		var y = x.toArray
		var d = 1
		for (n <- 0 until c.Dim) {
			y(n) += d //+ 1
			if (y(n) == c.K(n)) {
				y(n) = 0
			} else {
				d = 0
			}
		}
		// println("y: " + stringOf(y))//y.deep.mkString)
		y.toVector
	}


	def PortToDest( portNum : Int ) : Vector[Int] = {
		
		var cord = Vector[Int](0,0,0,0)
		for (i <- 0 until portNum){
			cord = IncrementCoord(cord)
		}
		cord
	}

	def DestToPort (dest : Vector[Int]) : Int = {
			var port = dest(0) + (dest(1)*c.K(1))
			port
	}	

	
	val iterationCount		= 5 //if this number gets too high (above 64 for a 2x2, (max packet ID  = 256, so 256 / 4 = 64) in the current test implementation we will overflow packetID and have false errors based on aliasing in the packetID map
	val packetLength		= 3
	var iterationCountPerPort 	= Array.fill(c.numPorts)(0)
	var packetIndexPerPort 		= Array.fill(c.numPorts)(0)
	//val dests 			= Array.fill(c.numPorts * iterationCount)( (0,0) )
	//val packetIDs		= Array.fill(c.numPorts * iterationCount)( BigInt(0) )
	//val packetIDsInt	= Array.fill(c.numPorts * iterationCount)( UInt(0) )
	val dests 			= Array.ofDim[(Int,Int,Int)](c.numPorts, iterationCount)
	val packetIDs		= Array.ofDim[BigInt](c.numPorts, iterationCount)
	val packetIDsInt	= Array.ofDim[UInt](c.numPorts, iterationCount)
	val flits			= Array.ofDim[Array[BigInt]](c.numPorts, iterationCount, packetLength)
	var packetInjTime	= new HashMap[Int,Int]()
	var packetMap		= new HashMap[Int,Array[Int]]()
	var cycleCount		= 0

	var curID = 0	
	for(port <- 0 until c.numPorts){
	  for(iter <-0 until iterationCount) {
		dests(port)(iter)     = (Random.nextInt(c.K(0)), Random.nextInt(c.K(1)), Random.nextInt(c.C))
		packetIDs(port)(iter) = curID % 256
		println(packetIDs(port)(iter) )
		packetIDsInt(port)(iter) = UInt( curID % 256)
		curID += 1
		packetMap((packetIDs(port)(iter)).toInt) = Array.fill(4)(0)
		packetMap((packetIDs(port)(iter)).toInt)(0) = dests(port)(iter)._1
		packetMap((packetIDs(port)(iter)).toInt)(1) = dests(port)(iter)._2
		packetMap((packetIDs(port)(iter)).toInt)(2) = dests(port)(iter)._3
		packetMap((packetIDs(port)(iter)).toInt)(3) = 0
      }
	}

	//Set packet length to constant of 3 right now - one head, two body

	//set up all flits to be driven, store in flits() array
  for(iter <- 0 until iterationCount){
	for(port <- 0 until c.numPorts){
		printf("port: %d\n", port)
		
		//create a head and body flit for each port
		headFlitMap("Dest_0") 	= dests(port)(iter)._1
		headFlitMap("Dest_1") 	= dests(port)(iter)._2 
		headFlitMap("Dest_2") 	= dests(port)(iter)._3
		headFlitMap("Dest_3") 	= 0
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
	val totalCycleCount = 20*iterationCount //500
	val rfile = new File("./routerUtilOut.csv")
	val routerUtilFile = new BufferedWriter(new FileWriter(rfile))
	routerUtilFile.write("Router,Utilization")
	val cfile = new File("./channelUtilOut.csv")
	val channelUtilFile = new BufferedWriter(new FileWriter(cfile))
	channelUtilFile.write("Channel,Utilization")
	val lfile = new File("./latency.csv")
	val latencyUtilFile = new BufferedWriter(new FileWriter(lfile))
	latencyUtilFile.write("packetID,latency\n")
 	var routerStatStringCSV : String = ""	
 	var channelStatStringCSV : String = ""	
	for (r <- 0 until c.numRouters) {
		routerStatStringCSV = routerStatStringCSV + "Router "+ r + "," 
			for(i <- 0 until c.routerRadix) {
				channelStatStringCSV = channelStatStringCSV + "Router " + r + ":Ch " + i + ","
			}
	}
	routerUtilFile.write(routerStatStringCSV.dropRight(1) + "\n")
	channelUtilFile.write(channelStatStringCSV.dropRight(1) + "\n")
	routerStatStringCSV = ""
	channelStatStringCSV = ""	

  while((iterationCountPerPort.min < iterationCount) || (cycleCount < totalCycleCount)){
	for(port <- 0 until c.numPorts){

		//if credit.grant is low, then the port cannot accept flits (i.e. out of credits!)
		var packetReady = peek(c.io.ports(port).in.packetReady) 

		//port is ready, and this port is not yet done with all iterations
		if((packetReady > 0) && (iterationCountPerPort(port) < iterationCount)){
			//drive flit w/ flitValid
			var myFlit = flits(port)(iterationCountPerPort(port))(packetIndexPerPort(port))
			if(packetIndexPerPort(port) == 0){
				//var pID = (myFlit(0).toInt & 0x0003FC00) >> 10 //2x2
				//var pID = (myFlit(0).toInt & 0x0001FE000) >> 13 //4x4
				var pID = 	packetIDs(port)(iterationCountPerPort(port)).toInt 
				packetInjTime(pID)= cycleCount
				println("adding pid: ", pID)
			}
			poke(c.io.ports(port).in.packetValid, 1)
			poke(c.io.ports(port).in.packet, myFlit)
			
			//track where we are in this packet
			packetIndexPerPort(port)    = packetIndexPerPort(port) + 1

			//have we come to the end of the packet?
			if(packetIndexPerPort(port) == packetLength){
				packetIndexPerPort(port) = 0				
				iterationCountPerPort(port) = iterationCountPerPort(port) + 1
			}
		}else {
			//port not ready... drive all zeros and deassert flitValid
			poke(c.io.ports(port).in.packet, zeroFlit)
			poke(c.io.ports(port).in.packetValid, 0)
		}
	}
		var min = iterationCountPerPort.min
		println("iteratons remaining are: ", iterationCountPerPort(0), iterationCountPerPort(1), iterationCountPerPort(2), iterationCountPerPort(3)) 
		println("min is: ", min) 

	for(port <- 0 until c.numPorts){
		//check the scoreboard for valid flits from last cycle		
		if(validScoreboard(port)){
			var packetID = 0
			var dest = Vector(0,0,0,0)
			if(isHead(port)) {
				packetID = peek(c.io.flitsOutAsHead(port).packetID).toInt
				var portDest = PortToDest(port).toArray
				println("Found head flit", packetID)
			//	var expectedPort = DestToPort(Vector(packetMap(packetID)(0),packetMap(packetID)(1)))
				var expectedPort : Vector[Int] = Vector(peek(c.io.flitsOutAsHead(port).destination(0)).toInt, peek(c.io.flitsOutAsHead(port).destination(1)).toInt, peek(c.io.flitsOutAsHead(port).destination(2)).toInt,0)
				if (packetInjTime.contains(packetID)){
					var latency = cycleCount - packetInjTime(packetID)
					println("Latency for packet ID ", packetID, " is: ", latency)
					latencyUtilFile.write(packetID + "," + latency + "\n")
					//if (packetMap(packetID).deep != portDest.deep){
					if (packetMap(packetID).deep != expectedPort.toArray.deep){
			//		if (port != expectedPort){
						println("HEAD Flit expected to be " + packetMap(packetID).deep.mkString + " Instead, found: " + expectedPort.toArray.deep.mkString)
					//	println("HEAD Flit expected to be " + expectedPort + " Instead, found: " + port)
						expect(false, "Incorrect dest for head flit " + packetID.toString)
					}
				}
			} 
		}

		//look for valid flit on output port
		var validFlit = peek(c.io.ports(port).out.flitValid) > 0
		if(validFlit){
			//determine if flit is head, then translate it as appropriate 
			var myFlit = peek(c.io.ports(port).out.flit)
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
   step (1)
   cycleCount+=1

 	var statString : String = ""	
	routerStatStringCSV = ""
	channelStatStringCSV = ""	
	for (r <- 0 until c.numRouters) {
		var rb = peek(c.io.cyclesRouterBusy(r)) 
		statString = "Router " + r + " Stats:\tCycles Busy: " + rb + "\tChannel Busy stats: "
		routerStatStringCSV = routerStatStringCSV + (rb.toFloat/cycleCount)*100 + ","
		printf("%s\n",statString)
		for(i <- 0 until c.routerRadix) {
			var cb = peek(c.io.cyclesChannelBusy((r*c.routerRadix) + i))
			statString = statString + i +":" + cb + " " 
			channelStatStringCSV =  channelStatStringCSV + (cb.toFloat/cycleCount)*100 + "," 
		}
		printf("%s\n",statString)
	}
	routerUtilFile.write(routerStatStringCSV.dropRight(1) + "\n")
	channelUtilFile.write(channelStatStringCSV.dropRight(1) + "\n")

	
/*
	var statString : String = ""	
	for (r <- 0 until c.numRouters) {
		var rb = peek(c.io.cyclesRouterBusy(r)) 
		statString = "Router " + r + " Stats:\tCycles Busy: " + rb + "\tChannel Busy stats: "
		for(i <- 0 until c.routerRadix) {
			var cb = peek(c.io.cyclesChannelBusy((r*c.routerRadix) + i))
			statString = statString + i +":" + cb + " " 
		}
		printf("%s\n",statString)
	}*/

  }//end checking 

	step(50)
	routerUtilFile.close()
	channelUtilFile.close()
	latencyUtilFile.close()

	step(1)	
	
}


