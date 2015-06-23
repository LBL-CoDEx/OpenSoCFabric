package OpenSoC

import Chisel._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import scala.util.Random
import java.io._

class OpenSoC_CMeshTester_Combined_Packet_C2(c: OpenSoC_CMesh[Packet], parms: Parameters, testType : String) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

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
					("flitID"	-> 0 ),
					("payload"	-> 0 ),
					("vcPort"	-> 0 ),
					("isTail"	-> 0 ),
					("packetID"	-> 0 ) )
	
	poke(c.io.bodyFlitsIn(0), bodyFlitMap.values.toArray)

	step(1)
	var zeroFlit = peek(c.io.bodyFlitsOut(0))
	
	for (i <- 0 until c.numPorts) {
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
		
		var cord = Vector[Int](0,0,0)
		for (i <- 0 until portNum){
			cord = IncrementCoord(cord)
		}
		cord
	}

	def ComputeRandomDest( portNum : Int ) : (Int,Int) = {
		val dest = (Random.nextInt(c.K(0)), (Random.nextInt(c.K(1)))) //Random.nextInt(c.K.product)
		dest
	}

		

	
	val iterationCount		= 1 //if this number gets too high (above 64 for a 2x2, (max packet ID  = 256, so 256 / 4 = 64) in the current test implementation we will overflow packetID and have false errors based on aliasing in the packetID map
	val packetLength		= 8
	val payloadLength 		= 3
	var iterationCountPerPort 	= Array.fill(c.numPorts)(0)
	var packetIndexPerPort 		= Array.fill(c.numPorts)(0)
	//val dests 			= Array.fill(c.numPorts * iterationCount)( (0,0) )
	//val packetIDs		= Array.fill(c.numPorts * iterationCount)( BigInt(0) )
	//val packetIDsInt	= Array.fill(c.numPorts * iterationCount)( UInt(0) )
	val dests 			= Array.ofDim[Int](c.numPorts, iterationCount)
	val destsArr		= Array.ofDim[(Int,Int)](c.numPorts, iterationCount)
	val packetIDs		= Array.ofDim[BigInt](c.numPorts, iterationCount)
	val packetIDsInt	= Array.ofDim[Int](c.numPorts, iterationCount)
	val flits			= Array.ofDim[Array[BigInt]](c.numPorts, iterationCount, packetLength)
	val packets			= Array.ofDim[Array[BigInt]](c.numPorts, iterationCount, packetLength)
	var packetInjTime	= new HashMap[Int,Int]()
	var addrMap			= new HashMap[Int,Array[Int]]()
	var cycleCount		= 0

	var curID = 0	
	for(port <- 0 until c.numPorts){
	  for(iter <-0 until iterationCount) {
		if(testType == "Random"){
			destsArr(port)(iter)     = ComputeRandomDest(port)
			dests(port)(iter)     = destsArr(port)(iter)._1 | (destsArr(port)(iter)._2 << 4)
		}else if(testType == "Neighbor") {
			var destPort			= if(port < c.numPorts-1){PortToDest(port+1)}else{PortToDest(0)}
			dests(port)(iter)     = destPort(0) | (destPort(1) << 4)
			destsArr(port)(iter)     = (destPort(0), destPort(1))
		}else {
			println("ERROR: Unknown test type: " + testType)
		}


		packetIDs(port)(iter) = curID % 256
		println(packetIDs(port)(iter) )
		packetIDsInt(port)(iter) =  curID % 256
		curID += 1
		addrMap(packetIDsInt(port)(iter)) = Array.fill(3)(0)
		addrMap(packetIDsInt(port)(iter))(0) = destsArr(port)(iter)._1
		addrMap(packetIDsInt(port)(iter))(1) = destsArr(port)(iter)._2
		addrMap(packetIDsInt(port)(iter))(2) =  0
      }
	}

	val totalCycleCount = 20
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
		
		var inReady = peek(c.io.ports(port).in.packetReady)
		if((inReady > 0) && (iterationCountPerPort(port) < iterationCount)){
			var iter = iterationCountPerPort(port)
			poke(c.io.ports(port).in.packet.sourceAddress, (port | iterationCount << 16))
			poke(c.io.ports(port).in.packet.destAddress, (dests(port)(iter) << 16))
			poke(c.io.ports(port).in.packet.length(PacketFieldIndex.totalLength), packetLength)
			poke(c.io.ports(port).in.packet.length(PacketFieldIndex.payloadLength), payloadLength)
			poke(c.io.ports(port).in.packet.length(PacketFieldIndex.additionalFlags), 0)
			poke(c.io.ports(port).in.packet.command(PacketFieldIndex.packetID), packetIDs(port)(iter))
			poke(c.io.ports(port).in.packet.command(PacketFieldIndex.command), 0xA)
			poke(c.io.ports(port).in.packet.command(PacketFieldIndex.commandOptions), 0x5)
			poke(c.io.ports(port).in.packet.debug, 0)
			for(phase <- 0 until payloadLength){
				poke(c.io.ports(port).in.packet.payload(phase), 10*phase)
			}

			poke(c.io.ports(port).in.packetValid,1)
			packetInjTime(packetIDsInt(port)(iter))	= cycleCount
			println("STARTING PACKET")
			println("iteration count for port " + port + ": " + iterationCountPerPort(port))
			iterationCountPerPort(port) = iterationCountPerPort(port) + 1

		}else {
			poke(c.io.ports(port).in.packetValid,0)
	  	}
		var min = iterationCountPerPort.min
		println("min is: ", min) 
	}	


	for(port <- 0 until c.numPorts){
		//check the scoreboard for valid flits from last cycle		
		if(validScoreboard(port)){
			var packetID = 0
			var dest = Vector(0,0,0)
			if(isHead(port)) {
				packetID = peek(c.io.flitsOutAsHead(port).packetID).toInt
				var portDest = PortToDest(port).toArray
				println("Found head flit", packetID)
				//var packetID = peek(c.io.headFlitsOut(port))
				if (packetInjTime.contains(packetID)){
					var latency = cycleCount - packetInjTime(packetID)
					println("Latency for packet ID ", packetID, " is: ", latency)
					latencyUtilFile.write(packetID + "," + latency + "\n")
					//if (addrMap(packetID) != port){
					if (addrMap(packetID).deep != portDest.deep){
						println("HEAD Flit expected to be " + addrMap(packetID) + " Instead, found: " + port)
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


  }//end checking 

	step(30)
	routerUtilFile.close()
	channelUtilFile.close()
	latencyUtilFile.close()
	//expect(c.io.ports(1).out.flit, myHeadFlit)
	//expect(c.io.ports(1).out.flit, myBodyFlit)
/*
	//drive a flit on port 1
	headFlitMap("Dest_0") 	= 1
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
	expect(c.io.ports(3).out.flit, myHeadFlit)
	step(1)	
	expect(c.io.ports(3).out.flit, myBodyFlit)
	step(1)	
	*/
}


