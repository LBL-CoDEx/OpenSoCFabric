package OpenSoC

import Chisel._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import java.io._

class OpenSoC_CMesh_CombinedTester_VarInjRate(c: OpenSoC_CMesh[Flit], parms: Parameters, rate: Double, pattern: String, packetCountPerPort : Int, fragmentationFactor : Int) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0
//	implicit def int(x: Int): BigInt = x
  //	implicit def int(x: Bits): BigInt = x.litValue()

	reset(1)

	// var headFlitMap = LinkedHashMap[String, BigInt]()
	// var bodyFlitMap = LinkedHashMap[String, BigInt]()
	// headFlitMap = LinkedHashMap( 	
	// 				("Dest_0" 		-> 0 ),
	// 				("Dest_1" 		-> 0 ),
	// 				("Dest_2"		-> 0 ),
	// 				("packetType"	-> 0 ),
	// 				("vcPort"		-> 0 ),
	// 				("isTail"		-> 0 ),
	// 				("packetID"		-> 0 ) )
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
	//Random Test - Drive flits to random destinations 

	//Create Array of random destinations 
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

	def FlipCoin( portNum : Int) : Boolean = {

		val coinFlip = Random.nextInt(100)
		var okToInject : Boolean = false
		if (coinFlip <= rate){
			//if ((1.0 / cyclesSinceLastInjection(portNum)) <= rate ){
				okToInject = true
			//}
		}
		okToInject
	}

	def NeighborDest(portNum: Int) : Vector[Int] = {
		/*
		int NeighborTrafficPattern::dest(int source)
		{
			assert((source >= 0) && (source < _nodes));

			int offset = 1;
			int result = 0;

			for(int n = 0; n < _n; ++n) {
				result += offset * (((source / offset) % (_xr * _k) + 1) % (_xr * _k));
				offset *= (_xr * _k);
			}
			return result;
		}
		*/
		require(c.K.equals(Vector.fill(c.Dim)(c.K(0))), "Error: Neighbor traffic pattern requires a square network.")

		var dest = 0
		var offset = 1
		var xr : Int = scala.math.ceil(scala.math.sqrt(c.C)).toInt // Concentration Square Root
		
		for(n <- 0 until c.Dim){
			dest += (offset * ( ( ( portNum / offset) % (xr * c.K(0)) + 1) % (xr * c.K(0))) )
			offset *= xr * c.K(0)
		}
		val destCord = PortToDest(dest)
		destCord
	}

	def TornadoDest(portNum: Int) : Vector[Int] = {
		/*
		int TornadoTrafficPattern::dest(int source)
		{
			assert((source >= 0) && (source < _nodes));

			int offset = 1;
			int result = 0;

			for(int n = 0; n < _n; ++n) {
				result += offset * (((source / offset) % (_xr * _k) + ((_xr * _k + 1) / 2 - 1)) % (_xr * _k));
				offset *= (_xr * _k);
			}
			return result;
		}
		*/
		require(c.K.equals(Vector.fill(c.Dim)(c.K(0))), "Error: Tornado traffic pattern requires a square network.")

		var dest : Int = 0
		var offset : Int = 1
		var xr : Int = scala.math.ceil(scala.math.sqrt(c.C)).toInt // Concentration Square Root

		for(n <- 0 until c.Dim){
			dest += offset * ( ( (portNum / offset) % (xr * c.K(0)) + ( ( xr * c.K(0) + 1 ) / 2 - 1) ) % (xr * c.K(0)) );
			offset *= xr * c.K(0)
		}
		val destCord = PortToDest(dest)
		destCord
	}

	def TransposeDest(portNum : Int) : Vector[Int] = {
		/*
		TransposeTrafficPattern::TransposeTrafficPattern(int nodes)
			: BitPermutationTrafficPattern(nodes), _shift(0)
		{
			while(nodes >>= 1) {
				++_shift;
			}
			if(_shift % 2) {
				cout << "Error: Transpose traffic pattern requires the number of nodes to "
					<< "be an even power of two." << endl;
				exit(-1);
			}
			_shift >>= 1;
		}
		int TransposeTrafficPattern::dest(int source)
		{
			assert((source >= 0) && (source < _nodes));
			int const mask_lo = (1 << _shift) - 1;
			int const mask_hi = mask_lo << _shift;
			return (((source >> _shift) & mask_lo) | ((source << _shift) & mask_hi));
		}
		*/
		val total_nodes : Int = c.K.product * c.C
		val lg : Int = (scala.math.log(total_nodes) / scala.math.log(2)).toInt

		require((lg % 2) != 1, "Error: Transpose traffic pattern requires the number of nodes to be an even power of two.")

		val shift : Int = lg / 2
		val mask_lo : Int = (1 << shift) - 1
		val mask_hi : Int = mask_lo << shift

		val dest = (((portNum >> shift) & mask_lo) | ((portNum << shift) & mask_hi))

		val destCord = PortToDest(dest)
		destCord
	}

	def BitReverseDest(portNum: Int) : Vector[Int] = {
		/*
		int BitRevTrafficPattern::dest(int source)
		{
		  assert((source >= 0) && (source < _nodes));
		  int result = 0;
		  for(int n = _nodes; n > 1; n >>= 1) {
			result = (result << 1) | (source % 2);
			source >>= 1;
		  }
		  return result;
		}
		*/
		val total_nodes : Int	= c.K.product * c.C
		val lg : Int			= scala.math.ceil(scala.math.log(total_nodes) / scala.math.log(2)).toInt

		val loopList : Vector[Int] = (0 to lg).map(total_nodes >> _).toVector.filter(_ > 1)

		var dest : Int		= 0
		var source : Int	= portNum
		for(n <- loopList) {
			dest = (dest << 1) | (source % 2)
			source >>= 1
		}

		val destCord = PortToDest(dest % total_nodes)  // We add total_nodes here because this does not assume this is a sqaure network
		destCord
	}

	
	val iterationCount		= packetCountPerPort //if this number gets too high (above 64 for a 2x2, (max packet ID  = 256, so 256 / 4 = 64) in the current test implementation we will overflow packetID and have false errors based on aliasing in the packetID map
	var packetLength		= 15
	var iterationCountPerPort 	= Array.fill(c.numPorts)(0)
	var packetIndexPerPort 		= Array.fill(c.numPorts)(0)
	//val dests 			= Array.fill(c.numPorts * iterationCount)( (0,0) )
	//val packetIDs		= Array.fill(c.numPorts * iterationCount)( BigInt(0) )
	//val packetIDsInt	= Array.fill(c.numPorts * iterationCount)( UInt(0) )
	val dests 			= Array.ofDim[(Int,Int)](c.numPorts, iterationCount)
	val packetIDs		= Array.ofDim[BigInt](c.numPorts, iterationCount)
	val packetIDsInt	= Array.ofDim[UInt](c.numPorts, iterationCount)
	val flits			= Array.ofDim[Array[BigInt]](c.numPorts, iterationCount, packetLength)
	var packetInjTime	= new HashMap[Int,Int]()
	var packetMap		= new HashMap[Int,Array[Int]]()
    var packetLengths   = new HashMap[Int,Int]()
	var cycleCount		= 0
	var wonCoinToss	 = Array.fill(c.numPorts)(0)

	var curID = 0	
	for(port <- 0 until c.numPorts){
	  for(iter <-0 until iterationCount) {
        if(pattern == "BitReverse"){
		    dests(port)(iter)     = (BitReverseDest(port)(0),BitReverseDest(port)(1))
        }else if(pattern == "Neighbor"){
		    dests(port)(iter)     = (NeighborDest(port)(0),NeighborDest(port)(1))
        }else if(pattern == "Transpose"){
		    dests(port)(iter)     = (NeighborDest(port)(0),TransposeDest(port)(1))
        }else if(pattern == "Tornado"){
		    dests(port)(iter)     = (TornadoDest(port)(0),TornadoDest(port)(1))  
        }else if(pattern == "Random"){
		    dests(port)(iter)     = (Random.nextInt(c.K(0)), Random.nextInt(c.K(1)))
        }else{
			println("ERROR: Unknown test type: " + pattern)
		}
		packetIDs(port)(iter) = curID % 16384 
		println(packetIDs(port)(iter) )
		packetIDsInt(port)(iter) = UInt( curID % 16384)
		curID += 1
		packetMap((packetIDs(port)(iter)).toInt) = Array.fill(3)(0)
		packetMap((packetIDs(port)(iter)).toInt)(0) = dests(port)(iter)._1
		packetMap((packetIDs(port)(iter)).toInt)(1) = dests(port)(iter)._2
		packetMap((packetIDs(port)(iter)).toInt)(2) = 0
	  }
	}

	//Set packet length to constant of 3 right now - one head, two body

	//set up all flits to be driven, store in flits() array
  for(iter <- 0 until iterationCount){
	for(port <- 0 until c.numPorts){
		printf("port: %d\n", port)
		
        packetLength =  Random.nextInt(7)
        if(packetLength < 3){
            packetLength = 3
        }

		//create a head and body flit for each port
		headFlitMap("Dest_0") 	= dests(port)(iter)._1
		headFlitMap("Dest_1") 	= dests(port)(iter)._2 
		headFlitMap("Dest_2") 	= 0  // Need to fix for multiple concentrations
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
    
        packetLengths(packetIDs(port)(iter).toInt) = packetLength
        // Create enough flits to fill out packet
        for(f <- 0 until packetLength - 2) {     //Subtract two because we already created a head and a body
	
            if( (f+2) == (packetLength-1)){
           	    //setup the tail flit
		        bodyFlitMap("isTail") 	= 1
            }else{
		        bodyFlitMap("isTail") 	= 0
            }
		    poke(c.io.bodyFlitsIn(port), bodyFlitMap.values.toArray)
		    step(1)

		    //Add the tail flit to the array of flits
		    myBodyFlit = peek(c.io.bodyFlitsOut(port))
		    flits(port)(iter)(f+2) = myBodyFlit
        }
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
		val readyForInj = FlipCoin(port)
		if (readyForInj && (packetReady == 0) ){
			wonCoinToss(port) += 1
		}

		if((packetReady > 0) && ((cycleCount % fragmentationFactor) == 0) && (iterationCountPerPort(port) < iterationCount) && (readyForInj || (wonCoinToss(port) > 0) || (packetIndexPerPort(port) > 0))){
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
			packetIndexPerPort(port)	= packetIndexPerPort(port) + 1

			//have we come to the end of the packet?
			//if(packetIndexPerPort(port) == packetLength){
			if(packetIndexPerPort(port) == packetLengths(packetIDs(port)(iterationCountPerPort(port)).toInt) ){
				packetIndexPerPort(port) = 0				
				iterationCountPerPort(port) = iterationCountPerPort(port) + 1
				wonCoinToss(port) -= 1
				if (wonCoinToss(port) < 0) {wonCoinToss(port) = 0}
			}
		}else {
			if( (packetReady == 0) && (packetIndexPerPort(port) > 0)) {
				println("Splitting Packet " + packetIDs(port)(iterationCountPerPort(port)).toInt)
			}
			//port not ready... drive all zeros and deassert flitValid
			poke(c.io.ports(port).in.packet, zeroFlit)
			poke(c.io.ports(port).in.packetValid, 0)
		}
	}
		var min = iterationCountPerPort.min
		println("iteratons remaining are: ", iterationCountPerPort(0), iterationCountPerPort(1), iterationCountPerPort(2), iterationCountPerPort(3)) 
		println("min is: ", min) 

    val injQueueLatency = 2
	for(port <- 0 until c.numPorts){
		//check the scoreboard for valid flits from last cycle		
		if(validScoreboard(port)){
			var packetID = 0
			var dest = Vector(0,0,0)
			if(!isHead(port)) {
				packetID = peek(c.io.flitsOutAsBody(port).packetID).toInt
				var portDest = PortToDest(port).toArray
				//var packetID = peek(c.io.headFlitsOut(port))
                if(peek(c.io.flitsOutAsBody(port).isTail).toInt > 0){
				    println("Found tail flit", packetID)
    				if (packetInjTime.contains(packetID)){
			    		var latency = cycleCount - packetInjTime(packetID) - injQueueLatency - 1 // subtract 1 becasue the flit actually arrived last cycle
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
			//if((myFlit(0).toInt & (0x1 << 10)) > 0 ) {
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

	step(1)
	routerUtilFile.close()
	channelUtilFile.close()
	latencyUtilFile.close()
	step(1)	
	
}


