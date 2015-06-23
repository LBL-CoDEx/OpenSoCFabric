package OpenSoC

import Chisel._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.util.control._
import scala.io.Source
import scala.util.Random
import java.io._

class OpenSoC_CMesh_TraceTester(c: OpenSoC_CMesh[Flit], parms: Parameters, traceFilename: String, linesToProcess: Int) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	reset(1)

	var headFlitMap : LinkedHashMap[String, BigInt] = LinkedHashMap[String, BigInt]() ++ (
		(0 to c.Dim).map(i => ("Dest_"+i.toString -> BigInt(0))) ++ List[(String, BigInt)](
			("packetType"	-> BigInt(0) ),
			("vcPort"		-> BigInt(0) ),
			("isTail"		-> BigInt(0) ),
			("packetID"		-> BigInt(0) ) )
		)

	var bodyFlitMap : LinkedHashMap[String, BigInt] = LinkedHashMap[String, BigInt]() ++ List[(String, BigInt)](
					("payload"	-> BigInt(0) ),
					("flitID"	-> BigInt(0) ),
					("vcPort"	-> BigInt(0) ),
					("isTail"	-> BigInt(0) ),
					("packetID"	-> BigInt(0) ) )
	
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

	var validScoreboard = Array.fill( c.numPorts ){ false }
	var isHead			= Array.fill( c.numPorts ){ false }

	def PortToDest( portNum : Int ) : Vector[Int] = {
		// var coord : Vector[Int] = Vector.fill(Dim)(0)
		var coord : ArrayBuffer[Int] = ArrayBuffer[Int]()
		var portIterator : Int = portNum

		portIterator /= c.C
		for (i <- 0 until c.Dim) {
			coord.append(portIterator % c.K(i))
			portIterator /= c.K(i)
		}
		coord.append(portNum % c.C)
		
		coord.toVector
	}

	
    val lineCount           = linesToProcess
	val packetLength		= 8
	var iterationCountPerPort 	= Array.fill(c.numPorts)(0)
	var packetIndexPerPort 		= Array.fill(c.numPorts)(0)
	//val packetIDs		= Array.ofDim[BigInt](c.numPorts, iterationCount)
	val packetIDs		= new HashMap[Int, Array[BigInt]]()
	var packetInjTime	= new HashMap[Int,Int]()
	var packetMap		= new HashMap[Int,Array[Int]]()
	var cycleCount		= 0


    val flitMap             = new HashMap[Int, Array[ListBuffer[Array[BigInt]]]]() 
    val timeCol             = 1
    val sourceCol           = 2
    val destCol             = 3
    val sizeCol             = 4
    val timeFactor          = 1000000
    val maxPacketLength     = 8
    val bytesPerFlit        = 8
	var curID               = 0	
    var curLineNum          = 0
    var totalFlits          = 0

	//set up all flits to be driven, store in flitMap 
   val loop = new Breaks;
   loop.breakable {
   for(line <- Source.fromFile(traceFilename).getLines) { 
    
    if (curLineNum > lineCount) { loop.break }
    curLineNum += 1

    val packetInfo  = line.split(" ").map(_.trim)
    
    val time        = packetInfo(timeCol).toInt   / timeFactor
    val source      = packetInfo(sourceCol).toInt % c.numPorts
    val dest        = PortToDest(packetInfo(destCol).toInt   % c.numPorts)
    var flitCount   = (packetInfo(sizeCol).toInt   / bytesPerFlit)  + 2
    val packetCount = flitCount / maxPacketLength
    var headFlit    = false
    var isTail      = 0
    if(!flitMap.contains(time)){
     flitMap(time)     = Array.fill(c.numPorts)(ListBuffer[Array[BigInt]]())
    }

    for (flit <- 0 until flitCount) {
                
        totalFlits += 1

		//create a head and body flit for each port
        if((flit % maxPacketLength) == 0){
	    	headFlitMap("Dest_0") 	= dest(0)
    		headFlitMap("Dest_1") 	= dest(1) 
	    	headFlitMap("Dest_2") 	= 0  // Need to fix for multiple concentrations
	    	headFlitMap("isTail") 	= 0
    		headFlitMap("packetID") = curID
		    headFlit                = true
            isTail                  = 0 //if(flit == (flitCount - 1)) {1} else {0}
            packetMap(curID)        = dest.toArray
        }else{
            isTail                   =  if ((flit == (flitCount-1)) || ((flit % maxPacketLength) == (maxPacketLength-1)) ){ 1 } else { 0 }
    		bodyFlitMap("packetID") = curID
	    	bodyFlitMap("isTail") 	= isTail
            headFlit                = false
        }
            
	    poke(c.io.headFlitsIn(source), headFlitMap.values.toArray)
        poke(c.io.bodyFlitsIn(source), bodyFlitMap.values.toArray)

		step(1)

		//get head and body flits from extractor blocks, then add to array based on port & iteration#
		var myHeadFlit = peek(c.io.headFlitsOut(source))
		var myBodyFlit = peek(c.io.bodyFlitsOut(source))
		if(headFlit){
            flitMap(time)(source) += myHeadFlit
            println("DEBUG: Adding head flit for port " + source.toString + " time:" + time.toString + " ID:" + curID + " isTail:" + isTail.toString)
            if(packetIDs.contains(time)){
                packetIDs(time)(source) = curID
            }else{
                packetIDs(time) = Array.fill(c.numPorts)(BigInt(0))
                packetIDs(time)(source) = curID
            }
                
        }else{
            println("DEBUG: Adding body flit for port " + source.toString + " time: " + time.toString + " ID:" + curID.toString + " isTail:" + isTail.toString)
		    flitMap(time)(source) += myBodyFlit
        }

        if(headFlit && (flit == (flitCount -1))){
            bodyFlitMap("packetID") = curID
	    	bodyFlitMap("isTail") 	= 1
            poke(c.io.bodyFlitsIn(source), bodyFlitMap.values.toArray)
            step(1)
		    myBodyFlit = peek(c.io.bodyFlitsOut(source))
            println("DEBUG: Adding body flit for port " + source.toString + " time: " + time.toString + " ID:" + curID.toString + " isTail:" + isTail.toString)
		    flitMap(time)(source) += myBodyFlit
            isTail = 1
        }

        if(isTail > 0){
            curID += 1
        }

	   }
	 }
   }

	//begin driving flits on all ports
	val totalCycleCount = 20*totalFlits//500
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
   
    var curFlit = 0 
    val timesToProcessPerPort   = HashMap[Int,ListBuffer[Int]]() 

    for (port <- 0 until c.numPorts){
        timesToProcessPerPort(port) = ListBuffer[Int]()
    }


  while((curFlit < totalFlits) || (cycleCount < totalCycleCount)){

    println("DEBUG: totalFlits: " + totalFlits.toString + " curFlit: " + curFlit) 
        if(flitMap.contains(cycleCount)){
            var portNum = 0
            for (port <- flitMap(cycleCount)){
                 if(!port.isEmpty){
                    timesToProcessPerPort(portNum) += cycleCount
                    println("DEBUG: found " + port.length + " flits to process at time " + cycleCount.toString + " for port " + portNum.toString)
                }
                portNum += 1
            }
        }

	    for(port <- 0 until c.numPorts){

		//if credit.grant is low, then the port cannot accept flits (i.e. out of credits!)
		var packetReady = peek(c.io.ports(port).in.packetReady) 

		if((packetReady > 0) && !timesToProcessPerPort(port).isEmpty) {
			//drive flit w/ flitValid
            var time = timesToProcessPerPort(port)(0)
            println("DEBUG: ready to drive flits on port " + port.toString + " at time " + time )
			var myFlit = flitMap(time)(port).remove(0)
			if(packetIndexPerPort(port) == 0){
				var pID = 	packetIDs(timesToProcessPerPort(port)(0))(port).toInt 
				packetInjTime(pID)= cycleCount
				println("DEBUG: adding pid: ", pID)
			}

			poke(c.io.ports(port).in.packetValid, 1)
			poke(c.io.ports(port).in.packet, myFlit)
			
			//have we come to the end of the packet?
            if(flitMap(time)(port).isEmpty){ 
				timesToProcessPerPort(port).remove(0)
                packetIndexPerPort(port) = 0
			} else{
                packetIndexPerPort(port) += 1
            }
            curFlit += 1

		}else {
			//port not ready... drive all zeros and deassert flitValid
			poke(c.io.ports(port).in.packet, zeroFlit)
			poke(c.io.ports(port).in.packetValid, 0)
		}
	}

	for(port <- 0 until c.numPorts){
		//check the scoreboard for valid flits from last cycle		
		if(validScoreboard(port)){
			var packetID = 0
			var dest = Vector(0,0,0)
			if(!isHead(port)) {
				packetID = peek(c.io.flitsOutAsBody(port).packetID).toInt
				var portDest = PortToDest(port).toArray
                if(peek(c.io.flitsOutAsBody(port).isTail).toInt > 0){
				    println("Found tail flit", packetID)
    				if (packetInjTime.contains(packetID)){
			    		var latency = cycleCount - packetInjTime(packetID)
		    			println("Latency for packet ID ", packetID, " is: ", latency)
	    				latencyUtilFile.write(packetID + "," + latency + "\n")
    					if (packetMap(packetID).deep != portDest.deep){
			    			println("HEAD Flit expected to be " + packetMap(packetID).deep.mkString + " Instead, found: " + portDest.deep.mkString)
		    				expect(false, "Incorrect dest for head flit " + packetID.toString)
	    				}
    				}
                }
			} 
		}

		//look for valid flit on output port
		var validFlit = peek(c.io.ports(port).out.flitValid) > 0
		if(validFlit){
			//determine if flit is tail, then translate it as appropriate 
			var myFlit = peek(c.io.ports(port).out.flit)
			if((myFlit(0).toInt & 0x1) > 0 ) {
				isHead(port) = true
			}else{
				poke(c.io.flitsIn(port), myFlit)
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

	step(50)
	routerUtilFile.close()
	channelUtilFile.close()
	latencyUtilFile.close()

	
}


