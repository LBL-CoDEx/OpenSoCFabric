package OpenSoC

import Chisel._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.util.Random

class OpenSoC_CMeshTester_Neighbor(c: OpenSoC_CMesh[Flit], parms: Parameters) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0
/*
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
	
	for (i <- 0 until c.numPorts) {
		poke(c.io.ports(i).in.flit, zeroFlit)
		poke(c.io.ports(i).in.credit.ready, 0)
		poke(c.io.ports(i).out.credit.valid, 0)
		
	}
	step(1)

	//--------------------------------------------------
	//Neighbor Test - Drive flits to neighboring router 

	//Create Array of destinations 
	val iterationCount 	= 5
	val Dim = 2
	val routerDim		= c.K    
	val routerXDim		= c.K(0)
	val routerYDim		= c.K(1)
	var dests			= Array.ofDim[Int](iterationCount, c.Dim, routerDim.max)
	var packetIDs	 	= Array.ofDim[Int](iterationCount, c.Dim, routerDim.max)
	var packetMap		= new HashMap[(Int,Int,Int),Int]	
		
  	for( iter <- 0 until iterationCount) {
		for( x<- 0 until routerDim(0)) {
			for(y <- 0 until routerDim(1)){
				dests(iter)(x)(y) = if (y == (routerDim(1) - 1) ) 0 else y + 1
				packetIDs(iter)(x)(y) = ((x*routerDim(0) + y) * (iter + 1)) % 256
				packetMap((iter,x,y)) = dests(iter)(x)(y)
			}
		}
  	}
	
// (0,0) (0,1) (0,2) (0,3)
// (1,0) (1,1) (1,2) (1,3)
// (2,0) (2,1) (2,2) (2,3)
// (3,0) (3,1) (3,2) (3,3)

	//Set packet length to constant of 3 right now - one head, two body

  for(iter <- 0 until iterationCount){
	for(x <- 0 until routerDim(0)){
		for(y <- 0 until routerDim(1)){
		
		//create a head and body flit for each port
		headFlitMap("Dest_0") 	= x
		headFlitMap("Dest_1") 	= dests(iter)(x)(y) 
		headFlitMap("Dest_2") 	= 0
		headFlitMap("isTail") 	= 0
		headFlitMap("packetID") = packetIDs(iter)(x)(y)
		bodyFlitMap("packetID") = packetIDs(iter)(x)(y)
		bodyFlitMap("isTail") 	= 0
		poke(c.io.headFlitsIn((routerDim(0)*x) + y), headFlitMap.values.toArray)
		poke(c.io.bodyFlitsIn((routerDim(0)*x) + y), bodyFlitMap.values.toArray)
	   }
	}

	step(1)

	//Drive all head flits	
	for(port <-0 until c.numPorts){
		var myHeadFlit = peek(c.io.headFlitsOut(port))
		poke(c.io.ports(port).in.credit.ready, 1)
		poke(c.io.ports(port).in.flit, myHeadFlit)

	}
	step(1)

	//Drive all body flits and create next body flit (tail)
	for(port <-0 until c.numPorts){
		var myBodyFlit = peek(c.io.bodyFlitsOut(port))
		poke(c.io.ports(port).in.credit.ready, 1)
		poke(c.io.ports(port).in.flit, myBodyFlit)

		bodyFlitMap("isTail") 	= 1
		poke(c.io.bodyFlitsIn(port), bodyFlitMap.values.toArray)	
	}
	
	step(1)
	
	for(port <-0 until c.numPorts){
		var myBodyFlit = peek(c.io.bodyFlitsOut(port))
		poke(c.io.ports(port).in.credit.ready, 1)
		poke(c.io.ports(port).in.flit, myBodyFlit)
	}

	step(1)

	for(port <-0 until c.numPorts){
		poke(c.io.ports(port).in.flit, zeroFlit)
		poke(c.io.ports(port).in.credit.ready, 0)
	}
		
  }

	step(48)
	
	//start checking process...
	var validScoreboard = Array.fill( c.numPorts ){ false }
	var isHead			= Array.fill( c.numPorts ){ false }
  *//*for(clkCount <- 0 until 48){
	for(port <- 0 until c.numPorts){
		//check the scoreboard for valid flits from last cycle		
		if(validScoreboard(port)){
			var packetID = UInt(0)
			if(isHead(port)) {
				val hf = Flit.fromBits(UInt(peek(c.io.headFlitsOut(port))(0).toInt), parms).asHead()
				packetID = hf.packetID

			} else {
				val bf = Flit.fromBits(UInt(peek(c.io.bodyFlitsOut(port))(0).toInt), parms).asBody()
				packetID = bf.packetID
				}
			var good =  port == packetMap(packetID)
			expect(good, "Packet found")
		}

		//look for valid flit on output port
		var validFlit = peek(c.io.ports(port).out.credit.ready) > 0
		if(validFlit){
			//determine if flit is head, then translate it as appropriate 
			var myFlit = peek(c.io.ports(port).out.flit)
			if((myFlit(0) & 0x1) > 0) {
				poke(c.io.headFlitsIn(port), myFlit)
			}else{
				poke(c.io.bodyFlitsIn(port), myFlit)
			}
			//mark scoreboard so flit can be checked on the next clock
			validScoreboard(port) = true
		}else{
			validScoreboard(port) = false
		}
	}
   step (1)
  }*/
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
		poke(c.io.ports(i).in.credit.ready, 0)
		poke(c.io.ports(i).out.credit.valid, 0)
	}
	poke(c.io.ports(1).in.credit.ready, 1)
	poke(c.io.ports(1).in.flit, myHeadFlit)
	step(1)
	poke(c.io.ports(1).in.credit.ready, 1)
	poke(c.io.ports(1).in.flit, myBodyFlit)
	step(1)
	poke(c.io.ports(1).in.flit, zeroFlit)
	poke(c.io.ports(1).in.credit.ready, 0)
	step(6)
	expect(c.io.ports(3).out.flit, myHeadFlit)
	step(1)	
	expect(c.io.ports(3).out.flit, myBodyFlit)
	step(1)	
	*/
}


