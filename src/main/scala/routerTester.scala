package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.util.Random
import Array._

class SimpleVCRouterTestWrapper(parms: Parameters) extends Module(parms){

	val numInChannels 	= parms.get[Int]("numInChannels")
	val numOutChannels 	= parms.get[Int]("numOutChannels")
	val topoInCredits 	= parms.get[Int]("routerInCredits")
	val topoOutCredits 	= parms.get[Int]("routerOutCredits")
	
	val routerCtor 		= parms.get[Parameters=>Router]("routerCtor")
	val routingFuncCtor 	= parms.get[Parameters=>RoutingFunction]("rfCtor")

	val io = new Bundle {
		val inChannels  = Vec.fill(numInChannels) { new Channel(parms) }
		val outChannels = Vec.fill(numOutChannels) { new Channel(parms).flip() }
		val headFlitIn  = new HeadFlit(parms).asInput
		val headFlitOut = new Flit(parms).asOutput
		val bodyFlitIn  = new BodyFlit(parms).asInput
		val bodyFlitOut = new Flit(parms).asOutput
	}

	val flitExtract = Chisel.Module( new Flit2FlitBundle(parms) )
	val headExtract = Chisel.Module( new HeadBundle2Flit(parms) )
	val bodyExtract = Chisel.Module( new BodyBundle2Flit(parms) )
	val Router	= Chisel.Module ( routerCtor(
				parms.child(("Router"), Map(
					("numInChannels"->Soft(numInChannels)),
					("numOutChannels"->Soft(numOutChannels)),
					("routerInCredits"->Soft(topoInCredits)),
					("routerOutCredits"->Soft(topoOutCredits)),
					("rfCtor"->Soft(routingFuncCtor))
				))))

	io.headFlitIn 	<> headExtract.io.inHead
	io.headFlitOut  <> headExtract.io.outFlit
	io.bodyFlitIn 	<> bodyExtract.io.inBody
	io.bodyFlitOut  <> bodyExtract.io.outFlit
	
	for( i <- 0 until numInChannels) {
		io.inChannels(i) <> Router.io.inChannels(i)
	}

	for (i <- 0 until numOutChannels){
		io.outChannels(i) <> Router.io.outChannels(i)
	}
}
	

class SimpleVCRouterTester (c: SimpleVCRouterTestWrapper) extends Tester(c) {
	val routerLatencyInClks = 3

	var headFlitMap = LinkedHashMap[String, BigInt]()
	var bodyFlitMap = LinkedHashMap[String, BigInt]()
	headFlitMap     = LinkedHashMap(
		("Dest_0" 		-> 0 ),
		("Dest_1" 		-> 0 ),
		("Dest_2"		-> 0 ),
		("packetType"	-> 0 ),
		("isTail"		-> 1 ),
		("packetID"		-> 0 )
	)

	bodyFlitMap 	= LinkedHashMap(
		("payload"		-> 0xDEAD ),
		("flitID"		-> 0xC ),
		("isTail"		-> 0 ),
		("packetID"		-> 0 )
	)
	
	poke(c.io.headFlitIn, headFlitMap.values.toArray)
	poke(c.io.bodyFlitIn, bodyFlitMap.values.toArray)
	step(1)
	var zeroFlit = peek(c.io.bodyFlitOut)

	for (i <- 0 until c.numInChannels) {
		poke(c.io.inChannels(i).flitValid,  0)
		poke(c.io.inChannels(i).credit.grant,  0)
		// poke(c.io.inChannels(i).credit.isTail, 0)
	}
	step(1)
	printf("-------------------- Test 1 ----------------------\n")
	printf("Drive Simple 2-flit packet from port 0 to port 1\n")
	//drive a flit on port 0
	headFlitMap("Dest_0") 	= 1
	headFlitMap("Dest_1") 	= 0
	headFlitMap("Dest_2") 	= 0
	headFlitMap("isTail") 	= 0
	headFlitMap("packetID") = 3
	bodyFlitMap("packetID") = 3
	bodyFlitMap("isTail") 	= 1
	poke(c.io.headFlitIn, headFlitMap.values.toArray)
	poke(c.io.bodyFlitIn, bodyFlitMap.values.toArray)
	step(1)
	var myHeadFlit = peek(c.io.headFlitOut)
	var myBodyFlit = peek(c.io.bodyFlitOut)
	
	step(1)
	for (i <- 0 until c.numInChannels) {
		poke(c.io.inChannels(i).flitValid, 0)
		poke(c.io.outChannels(i).credit.grant, 0)
	}
	poke(c.io.inChannels(0).flitValid, 1)
	poke(c.io.inChannels(0).flit, myHeadFlit)
	step(1)
	poke(c.io.inChannels(0).flitValid, 1)
	poke(c.io.inChannels(0).flit, myBodyFlit)
	step(1)
	poke(c.io.inChannels(0).flit, zeroFlit)
	poke(c.io.inChannels(0).flitValid, 0)
	step(routerLatencyInClks-2)
	expect(c.io.outChannels(1).flit, myHeadFlit)
	step(1)
	expect(c.io.outChannels(1).flit, myBodyFlit)
	printf("------------------ END Test 1 ---------------------\n\n")
	
	step(1)
	printf("-------------------- Test 1.5 ----------------------\n")
	printf("Drive Simple 3-flit packet from Router (0,0) to Router (1,1) (port 0 to port 1)\n")
	//drive a flit on port 0
	headFlitMap("Dest_0") 	= 0
	headFlitMap("Dest_1") 	= 1
	headFlitMap("Dest_2") 	= 0
	headFlitMap("isTail") 	= 0
	headFlitMap("packetID") = 3
	bodyFlitMap("packetID") = 3
	bodyFlitMap("isTail") 	= 0
	poke(c.io.headFlitIn, headFlitMap.values.toArray)
	poke(c.io.bodyFlitIn, bodyFlitMap.values.toArray)
	step(1)
	myHeadFlit = peek(c.io.headFlitOut)
	myBodyFlit = peek(c.io.bodyFlitOut)
	step(1)
	bodyFlitMap("isTail") 	= 1
	poke(c.io.bodyFlitIn, bodyFlitMap.values.toArray)
	step(1)
	var my2ndBodyFlit = peek(c.io.bodyFlitOut)
	
	step(1)
	for (i <- 0 until c.numInChannels) {
		poke(c.io.inChannels(i).flitValid, 0)
		poke(c.io.outChannels(i).credit.grant, 0)
	}
	poke(c.io.inChannels(0).flitValid, 1)
	poke(c.io.inChannels(0).flit, myHeadFlit)
	step(1)
	poke(c.io.inChannels(0).flitValid, 1)
	poke(c.io.inChannels(0).flit, myBodyFlit)
	step(1)
	poke(c.io.inChannels(0).flitValid, 1)
	poke(c.io.inChannels(0).flit, my2ndBodyFlit)
	step(1)
	poke(c.io.inChannels(0).flit, zeroFlit)
	poke(c.io.inChannels(0).flitValid, 0)
	expect(c.io.outChannels(3).flit, myHeadFlit)
	step(1)
	expect(c.io.outChannels(3).flit, myBodyFlit)
	step(1)
	expect(c.io.outChannels(3).flit, my2ndBodyFlit)
	printf("------------------ END Test 1.5 ---------------------\n\n")
	step(5)
	
	printf("-------------------- Test 2 ----------------------\n")
	printf("Drive 2-flit packets on each port, with destination\n")
	printf("     of neighbor port\n")	
	
	//Create an array of 2-flit packets:
	var packets = ofDim[Array[BigInt]](c.numInChannels,2)
	val dest    =  Array(3, 0, 1, 2, 3)
	for(i <- 0 until c.numInChannels){
		headFlitMap("Dest_0") 	= dest(i) & 1 
		headFlitMap("Dest_1") 	= (dest(i) & 2) >> 1 
		headFlitMap("Dest_2") 	= 0
		headFlitMap("isTail") 	= 0
		headFlitMap("packetID") = i 
		bodyFlitMap("packetID") = i
		bodyFlitMap("isTail") 	= 1
		poke(c.io.headFlitIn, headFlitMap.values.toArray)
		poke(c.io.bodyFlitIn, bodyFlitMap.values.toArray)
		printf("Dest: %d, %d, %d\n", headFlitMap("Dest_0"), headFlitMap("Dest_1"), headFlitMap("Dest_2"))
		step(1)
		packets(i)(0) = peek(c.io.headFlitOut)
		packets(i)(1) = peek(c.io.bodyFlitOut)
	}
	
	//Drive all head flits	
	for(i <- 0 until c.numInChannels){
		poke(c.io.inChannels(i).flitValid, 1)
		poke(c.io.outChannels(i).credit.grant, 1)
		poke(c.io.inChannels(i).flit, packets(i)(0))
	}
	step (1)
	//Drive all body flits	
	for(i <- 0 until c.numInChannels){
		poke(c.io.inChannels(i).flitValid, 1)
		poke(c.io.outChannels(i).credit.grant, 1)
		poke(c.io.inChannels(i).flit, packets(i)(1))
	}
	step (1)
	for(i <- 0 until c.numInChannels){
		poke(c.io.inChannels(i).flitValid, 0)
		poke(c.io.outChannels(i).credit.grant, 0)
		poke(c.io.inChannels(i).flit, zeroFlit)
	}
	step(4)
	for (i <- 0 until c.numOutChannels){
		peek(c.io.outChannels(i).flit)	
	}
	step (1)
	for (i <- 0 until c.numOutChannels){
		peek(c.io.outChannels(i).flit)	
	}
	step (1)
	for (i <- 0 until c.numOutChannels){
		peek(c.io.outChannels(i).flit)	
	}
	step (1)
	for (i <- 0 until c.numOutChannels){
		peek(c.io.outChannels(i).flit)	
	}
	
	
	printf("------------------ END Test 2 ---------------------\n\n")
	
}

