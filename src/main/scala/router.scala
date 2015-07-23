package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.util.Random
import Array._

/*
abstract class RouterParams {
	val NumInChans = UInt()
	val NumOutChans = UInt()
	val RouteFunc = RoutingFunction() // Object
	val AllocatorType = AllocatorClass()
	val NumPipelineStages = UInt()
	
	// Potential Params for Child Classes
	// val NumVCs = UInt()
	// val InQDepth = UInt()
	// val OutQDepth = UInt()
	// val VCAllocatorType = AllocatorClass() 
}
*/

class CounterIO extends Chisel.Bundle {
	val counterVal = UInt(OUTPUT, width=32)
	val counterIndex = UInt(OUTPUT, width=8)
}

class RouterBuffer(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val enq   = Chisel.Decoupled( new Flit(parms) ).flip
		val deq   = Chisel.Decoupled( new Flit(parms) )
	}

	val bufferDepth = parms.get[Int]("bufferDepth")
	val queue = Chisel.Module ( new Chisel.Queue(new Flit(parms), bufferDepth) )
	queue.io.enq <> io.enq
	io.deq <> queue.io.deq
}

abstract class Router(parms: Parameters) extends Module(parms) {
	val numInChannels 	= parms.get[Int]("numInChannels")
	val numOutChannels 	= parms.get[Int]("numOutChannels")
	val io = new Bundle {
		val inChannels = Vec.fill(numInChannels) { new Channel(parms) }
		val outChannels = Vec.fill(numOutChannels) { new Channel(parms).flip() }
		val counters = Vec.fill(2) {new CounterIO }
	}
}

abstract class VCRouter(parms: Parameters) extends Module(parms) {
	val numInChannels 	= parms.get[Int]("numInChannels")
	val numOutChannels 	= parms.get[Int]("numOutChannels")
	val numVCs = parms.get[Int]("numVCs")
	
	val io = new Bundle {
		val inChannels = Vec.fill(numInChannels) { new ChannelVC(parms) }
		val outChannels = Vec.fill(numOutChannels) { new ChannelVC(parms).flip() }
		val counters = Vec.fill(2) {new CounterIO }
	}
}

class OpenSoC_ConstantEndpoint(parms: Parameters) extends Router(parms) {
	for (i <- 0 until numOutChannels) {
		io.outChannels(i).flit 		<> Flit.fromBits(UInt(0),parms)
		//io.inChannels(i).credit.grant 	:= UInt(0)
		io.outChannels(i).flitValid  := UInt(0)
	}
}

class OpenSoC_VCConstantEndpoint(parms: Parameters) extends VCRouter(parms) {
	for (i <- 0 until numOutChannels) {
		io.outChannels(i).flit 		<> Flit.fromBits(UInt(0),parms)
		//io.inChannels(i).credit.grant 	:= UInt(0)
		// io.outChannels(i).credit.map( _.ready := UInt(0) )
		io.outChannels(i).flitValid  := UInt(0)
	}
}

class SimpleRouterTestWrapper(parms: Parameters) extends Module(parms){

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
		val headFlitsIn  = Vec.fill(numInChannels) { new HeadFlit(parms).asInput }
		val headFlitsOut = Vec.fill(numInChannels) { new Flit(parms).asOutput }
		val bodyFlitsIn  = Vec.fill(numInChannels) { new BodyFlit(parms).asInput }
		val bodyFlitsOut = Vec.fill(numInChannels) { new Flit(parms).asOutput }
		val flitsIn		 = Vec.fill(numInChannels) { new Flit(parms).asInput }
		val flitsOutAsHead = Vec.fill(numInChannels) { new HeadFlit(parms).asOutput }
		val flitsOutAsBody = Vec.fill(numInChannels) { new BodyFlit(parms).asOutput }
	}

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

	for (port <- 0 until numInChannels){
		var headExtracter = Chisel.Module( new HeadBundle2Flit(parms) )
		var bodyExtracter = Chisel.Module( new BodyBundle2Flit(parms) )
		var flit2flit	  = Chisel.Module( new Flit2FlitBundle(parms) )
		io.headFlitsIn(port) 	<> 	headExtracter.io.inHead
		io.headFlitsOut(port)	<>	headExtracter.io.outFlit
		io.bodyFlitsIn(port)	<> 	bodyExtracter.io.inBody
		io.bodyFlitsOut(port)	<>	bodyExtracter.io.outFlit
		io.flitsIn(port)		<>  flit2flit.io.inFlit
		io.flitsOutAsHead(port)	<>  flit2flit.io.outHead
		io.flitsOutAsBody(port)	<>  flit2flit.io.outBody
	}

	
	for( i <- 0 until numInChannels) {
		io.inChannels(i) <> Router.io.inChannels(i)
	}

	for (i <- 0 until numOutChannels){
		io.outChannels(i) <> Router.io.outChannels(i)
	}

}
	
//Current issue is router cannot handle back to back winners of two 3-length packets arriving staggered by one clock on different
//input buffers that are competing for the same output port.  Example:
// Head flit A arrives on port 0 at time 10, wins arbitration for output port 0 (flit home port)
// Head flit A wins arbitration for the next three cycles (10, 20 and 30)
// Head flit B arrives on port 1 at time 20, wins arbitration for output port 0 and *should* begin driving at time 40
// at time 40, switch select control (set by choosens) is set to drive from port 0, incorrectly driving input from port 0 to output port 0
// at time 50, switch select control correctly set to drive from port 1, allowing the two body flits from packet B to flow through

// Current thinking is this is due to a timing mis-match between the registered routing function result register, which controls the 
// arbiter request lines, and the input buffer deq ready signals


// Routers instantiate input buffers per input port, a switch, allocators and routing functions. They also instantiate logic to keep track of credits coming from output ports and keep track of that in input ports
class SimpleRouter(parms: Parameters) extends Router(parms) {
	val routerID		= parms.get[Vector[Int]]("routerID") //tuple
	val routerInCredits = parms.get[Int]("routerInCredits")
	val routerOutCredits = parms.get[Int]("routerOutCredits")
	val routingFuncCtor = parms.get[Parameters=>RoutingFunction]("rfCtor")
 	val numVCs = parms.get[Int]("numVCs")
	
	val numIns = numInChannels * numVCs

	//Create internal FUBs 
	val switch = Chisel.Module( new Switch[Flit](
		new Flit(parms), parms.child("Switch", Map(
			("numInPorts"->Soft(numIns)),
			("numOutPorts"->Soft(numOutChannels))
		))) )		
	val swAllocator = Chisel.Module( new SwitchAllocator(parms.child("SWAlloc", Map(
			("numReqs"->Soft(numIns)),
			("numRes"->Soft(numOutChannels)),
			("arbCtor"->Soft( (parms: Parameters) => new RRArbiter(parms) ))
		))) ) //Collection of arbiters
	switch.io.sel <> swAllocator.io.chosens

	//Router function is as follows:
		// a) Head flit enqued and passed to routing function 
		// b) Pass head flit to routing function
		// c) Routing function indicates what output resource should
		//		be requested 
		// d) Request is passed to allocator
		// e) Allocator will inform winner and set switch select
		// f) Data flows from input to output channel through switch
		// g) Credits are passed from the output back to the allocator
		//		once the resource is available again
	// Assumptions / requirements:
		// Data packets move as complete packets - no interleving 
		// Flits may be processed in any order
		// One routing function is instantiated per input to allow 
		//	simultaneous processing of requests

	//Loop through input channels, connect them to the routing functions 

	for (i <- 0 until numIns) {
		val creditGen = Chisel.Module ( new CreditGen(parms) )
		val headFlitRegFile = Chisel.Module ( new RouterRegFile(
			parms.child(("HeadFlitRegFile", i), Map(
				("depthRegFile"->Soft(routerInCredits)),
				("widthRegFile"->Soft(io.inChannels(0).flit.getWidth)),
				("pipelineDepth"->Soft(3))
			))) )
		val routingInBuffer = Chisel.Module ( new RouterBuffer(
			parms.child(("InBuffer", i), Map(
				("bufferDepth"->Soft(routerInCredits))
			))) )
		val routingFunction = Chisel.Module ( routingFuncCtor(
			parms.child(("RoutingFunction", i), Map(
				("routingCoord"->Soft(routerID)),
				("numResources"->Soft(numOutChannels))
			))) )
		
		// TAIL LOGIC
		val flitIsTail = routingInBuffer.io.deq.bits.isTail() && routingInBuffer.io.deq.valid
		val flitGranted = Bool(true) //orR(Vec((0 until numOutChannels).map(n => swAllocator.io.requests(n)(i).grant)).toBits)
		val noInputReq = UInt(1) //~orR(Vec((0 until numOutChannels).map(n => swAllocator.io.requests(n)(i).request)).toBits)
		val pop = (flitGranted && flitIsTail)// || noInputReq

		when(flitGranted) {
			io.counters(0).counterVal := UInt(1)
		}.otherwise {
			io.counters(0).counterVal := UInt(0)
		}


		creditGen.io.outCredit <> io.inChannels(i).credit
		// for ( (n,x) <- creditGen.io.outCredit.elements ) {
		// 	println("Element: " + n)
		// 	if (n == "ready")	creditGen.io.outCredit(n) := io.inChannels(i).credit(n)
		// 	else				io.inChannels(i).credit(n) := creditGen.io.outCredit(n)
		// }

	
		// io.inChannels(i).flit.whenHead { head => 
		// 	headFlitRegFile.io.writeData <> head
		// 	headFlitRegFile.io.writeEnable := routingInBuffer.io.enq.valid
		// } // NEEDS DEFAULT CASE, JOHN WILL IMPLIMENT
		when (io.inChannels(i).flit.isHead()) {
			headFlitRegFile.io.writeData := io.inChannels(i).flit.toBits
			headFlitRegFile.io.writeEnable := routingInBuffer.io.enq.valid
		} .otherwise {
			headFlitRegFile.io.writeData := UInt(0)
			headFlitRegFile.io.writeEnable := Bool(false)
		}
		headFlitRegFile.io.readIncrement := pop
		routingInBuffer.io.enq.bits := io.inChannels(i).flit
		routingInBuffer.io.enq.valid := io.inChannels(i).flitValid//creditGen.io.outReady
		creditGen.io.inGrant := flitGranted 


		val swAllocControl = Reg(init=UInt(0, width=Math.pow(2, routingFunction.io.result.getWidth).toInt )) 
		val swAllocControlInt = Reg(init=UInt(0, width=Math.pow(2, routingFunction.io.result.getWidth).toInt ))  
		val lockControl  = Reg(init=UInt(1, width=1 ))
		routingFunction.io.inHeadFlit := Flit.fromBits(headFlitRegFile.io.readData, parms).asHead()
		 when (headFlitRegFile.io.readValid && ~flitIsTail) {
			swAllocControl := UIntToOH(routingFunction.io.result) 
			swAllocControlInt := routingFunction.io.result
			lockControl  := pop  
		 }.otherwise{
			swAllocControl := UInt(0)
		 }
		
		for (j <- 0 until numOutChannels) {
			swAllocator.io.requests(j)(i).request 	:= swAllocControl(j).toBool 
			swAllocator.io.requests(j)(i).releaseLock := lockControl.toBool
		}

	//	val allocResultReg  = Reg(init=UInt(0, width=log2Up
		
		// Switch Input Logic
		//routingInBuffer.io.deq.ready := flitGranted  && orR(swAllocControl.toBits) 
		//Routing buffer deq should be asserted when:
			//input buffer has recieved a grant AND the switch allocator is driving the correct selects on the switch inputs
		//routingInBuffer.io.deq.ready := swAllocator.io.requests(swAllocControlInt)(i).grant  && (swAllocator.io.chosens(swAllocControlInt) === UInt(i)) //DDD
		when(routingInBuffer.io.deq.bits.isHead() && ~headFlitRegFile.io.readValid) {
			routingInBuffer.io.deq.ready := UInt(0) 
		}.elsewhen(routingInBuffer.io.deq.bits.isHead() && headFlitRegFile.io.readValid) {
			routingInBuffer.io.deq.ready := flitGranted 
		}.otherwise{
			routingInBuffer.io.deq.ready := swAllocator.io.requests(swAllocControlInt)(i).grant  && (swAllocator.io.chosens(swAllocControlInt) === UInt(i)) //DDD
		}
		// This is for adaptive and VC routing
		when(routingInBuffer.io.deq.bits.isHead() && headFlitRegFile.io.readValid) {
			switch.io.inPorts(i) <> headFlitRegFile.io.readData
		} .elsewhen(routingInBuffer.io.deq.valid) {
			switch.io.inPorts(i) <> routingInBuffer.io.deq.bits
		}.otherwise {
			switch.io.inPorts(i) <> UInt(0)
		}
	} //for (numIns)


	// Loop through output channels, connect to switch
	for (i <- 0 until numOutChannels) {
		val creditCon = Chisel.Module ( new CreditCon(parms.child(("CreditCon", i), Map(
				("numCreds"->Soft(routerInCredits))
			))) )
		val routingOutBuffer = Chisel.Module ( new RouterBuffer(
			parms.child(("OutBuffer", i), Map(
				("bufferDepth"->Soft(routerOutCredits))
			))) )
		swAllocator.io.resources(i).ready := routingOutBuffer.io.enq.ready
		routingOutBuffer.io.enq.bits <> switch.io.outPorts(i)
		when(switch.io.outPorts(i).isHead()){
			routingOutBuffer.io.enq.valid := UInt(0) //swAllocator.io.resources(i).valid  && orR(Vec((0 until numInChannels).map(n => swAllocator.io.requests(i)(n).grant)).toBits) 
		}.otherwise{
			routingOutBuffer.io.enq.valid := swAllocator.io.resources(i).valid  //&&  orR(Vec((0 until numInChannels).map(n => swAllocator.io.requests(i)(n).grant)).toBits) //Bug: valid goes high incorrectly, causing bad data to be latched
		}
		//creditCon.io.inValid := routingOutBuffer.io.deq.valid
		io.outChannels(i).flit := routingOutBuffer.io.deq.valid
		routingOutBuffer.io.deq.ready := creditCon.io.outCredit
		io.outChannels(i).flit <> routingOutBuffer.io.deq.bits
		creditCon.io.inCredit <> io.outChannels(i).credit
	}

}

class SimpleRouterTester (c: SimpleRouterTestWrapper) extends Tester(c) {
	val routerLatencyInClks = 3

	var headFlitMap = LinkedHashMap[String, BigInt]()
	var bodyFlitMap = LinkedHashMap[String, BigInt]()
	headFlitMap     = LinkedHashMap(
		("Dest_0" 		-> 0 ),
		("Dest_1" 		-> 0 ),
		("Dest_2"		-> 0 ),
		("packetType"	-> 0 ),
		("vcPort"		-> 0 ),
		("isTail"		-> 1 ),
		("packetID"		-> 0 )
	)

	bodyFlitMap 	= LinkedHashMap(
		("payload"		-> 0xDEAD ),
		("flitID"		-> 0xC ),
		("vcPort"		-> 0 ),
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

class SimpleVCRouter(parms: Parameters) extends VCRouter(parms) {
	val routerID		= parms.get[Vector[Int]]("routerID") //tuple
	val routerInCredits = parms.get[Int]("routerInCredits")
	val routerOutCredits = parms.get[Int]("routerOutCredits")
	val routingFuncCtor = parms.get[Parameters=>RoutingFunction]("rfCtor")
 	val numPriorityLevels = parms.get[Int]("numPriorityLevels") 

 	val numIns : Int = numInChannels*numVCs
 	val numOuts : Int = numOutChannels*numVCs

	//Create internal FUBs 
	val switch = Chisel.Module( new Switch[Flit](
		new Flit(parms), parms.child("Switch", Map(
			("numInPorts"->Soft(numIns)),
			("numOutPorts"->Soft(numOutChannels))
		))) )		
	val swAllocator = Chisel.Module( new SwitchAllocator(parms.child("SWAlloc", Map(
			("numReqs"->Soft(numIns)),
			("numRes"->Soft(numOutChannels)),
			("arbCtor"->Soft( (parms: Parameters) => new RRArbiterPriority(parms) ))
		))) ) //Collection of arbiters
	switch.io.sel <> swAllocator.io.chosens

	val vcAllocator = Chisel.Module( new SwitchAllocator(parms.child("VCAlloc", Map(
			("numReqs"->Soft(numOuts)),
			("numRes"->Soft(numIns)),
			("arbCtor"->Soft( (parms: Parameters) => new RRArbiter(parms) ))
		))) )
	val vcLockControls = (0 until numIns).map( c =>
		(0 until numOuts).map( d => 
			Reg(init=Bool(true))
		))
	val vcGrants = (0 until numIns).map( c =>
		(0 until numOuts).map( d => 
			vcAllocator.io.requests(d)(c).grant
			// Reg(init=Bool(false))
		))
	val flitsAreTail = (0 until numIns).map ( a => 
		Bool()
	)
	val rfResultsVC = (0 until numIns).map( a => 
		UInt(width=log2Up(numOutChannels))
	)

	val validVCs = (0 until numIns).map( a =>
		Vec.fill(numOutChannels) { Reg(UInt(0, width=numVCs)) }
	)

	val creditConsReady = Vec( (0 until numOutChannels).map( c =>
		Vec( (0 until numVCs).map(b =>
			Bool()
		) )
	) )

    val routerOutputStateMgmt = (0 until numInChannels).map( a => 
                Chisel.Module(new VCRouterOutputStateManagement(parms))
    ) 
    
    val currentRouterOutputState = Vec( (0 until numOutChannels).map (a =>
                 routerOutputStateMgmt(a).io.currentState
        ) )

    val readyToXmit = Vec ( ( 0 until numIns).map( a =>
            Vec ( ( 0 until numOutChannels).map( b =>
                 Bool()
            ) ) ) )
    readyToXmit.map (a => 
        a.map ( c => 
                c := Bool(false)
            )
        )
    val consumeCredit = Vec( (0 until numOutChannels).map( c =>
            Vec ( ( 0 until numVCs).map( b =>
    	        Bool()
            ) ) ) )
    consumeCredit.map( c =>
    	c := Bool(false)
    )
	
	//Router function is as follows:
		// a) Head flit enqued and passed to routing function 
		// b) Pass head flit to routing function
		// c) Routing function indicates what output resource should
		//		be requested 
		// d) Request is passed to allocator
		// e) Allocator will inform winner and set switch select
		// f) Data flows from input to output channel through switch
		// g) Credits are passed from the output back to the allocator
		//		once the resource is available again
	// Assumptions / requirements:
		// Data packets move as complete packets - no interleving 
		// Flits may be processed in any order
		// One routing function is instantiated per input to allow 
		//	simultaneous processing of requests

	//Loop through input channels, connect them to the routing functions 

	for (i <- 0 until numInChannels) {
		for (j <- 0 until numVCs) {
			val index = i*numVCs+j
            val curInVC         = j 
            val curInChannel    = i
			println("i: " + i + " (" + numInChannels + ")   j: " + j + "(" + numVCs + ")   index: " + index + "(" + (numIns) + ")")
			val creditGen = Chisel.Module ( new CreditGen(parms) )
			val headFlitRegFile = Chisel.Module ( new RouterRegFile(
				parms.child(("HeadFlitRegFile", i, j), Map(
					("depthRegFile"->Soft(routerInCredits)),
					("widthRegFile"->Soft(io.inChannels(0).flit.getWidth)),
					("pipelineDepth"->Soft(3))
				))) )
			val routingInBuffer = Chisel.Module ( new RouterBuffer(
				parms.child(("InBuffer", i, j), Map(
					("bufferDepth"->Soft(routerInCredits))
				))) )
			val routingFunction = Chisel.Module ( routingFuncCtor(
				parms.child(("RoutingFunction", i, j), Map(
					("routingCoord"->Soft(routerID)),
					("numResources"->Soft(numOutChannels))
				))) )

            val routerStateMgmt     = Chisel.Module(new VCRouterStateManagement(parms))

			val vcReplacer          = Chisel.Module( new ReplaceVCPort(parms.child( ("ReplaceVCPort",i,j), Map() )))
			val rfResult            = Reg(init=UInt(0, width=Math.pow(2, routingFunction.io.result.getWidth).toInt )) 
			val rfResultInt         = Reg(init=UInt(0, width=routingFunction.io.result.getWidth ))  
			val lockControl         = Reg(init=UInt(1, width=1 ))
            		val assignedVC          = Reg(init=UInt(0, width = io.inChannels(0).flit.getWidth))

			val flitPriority        = Reg(init=UInt(0, width=log2Up(numPriorityLevels)))
			val priorityLevel       = UInt(width=log2Up(numPriorityLevels))
			priorityLevel          := UInt(0)

            val VCRouterState       = new VCRouterState
            val VCRouterOutputState = new VCRouterOutputState
			
			// --- STATE MACHINE LOGIC ----
			flitsAreTail(index) := routingInBuffer.io.deq.bits.isTail() && routingInBuffer.io.deq.valid
			val flitIsTail       = flitsAreTail(index)
			val vcAllocGranted   = vcAllocator.io.resources(index).valid 
			val swAllocGranted   = swAllocator.io.requests(rfResultInt)(index).grant && (swAllocator.io.chosens(rfResultInt) === UInt(index)) 
			val flitGranted      = (swAllocGranted || ((routerStateMgmt.io.currentState === VCRouterState.swAllocGranted)) ) 
			val pop              = (flitGranted && flitIsTail) && creditConsReady(rfResultInt)(assignedVC)  && (routerStateMgmt.io.currentState === VCRouterState.swAllocGranted)

            val inValidReg       = Reg(init = Bool(false))
            inValidReg          := routingInBuffer.io.deq.valid 

            routerStateMgmt.io.inputBufferValid   := routingInBuffer.io.deq.valid
            routerStateMgmt.io.routingComplete    := inValidReg
            routerStateMgmt.io.vcAllocGranted     := vcAllocGranted
            routerStateMgmt.io.swAllocGranted     := swAllocGranted
            routerStateMgmt.io.creditsAvail       := creditConsReady(rfResultInt)(assignedVC)
            routerStateMgmt.io.inputBufferIsTail  := routingInBuffer.io.deq.valid && routingInBuffer.io.deq.bits.isTail()
            routerStateMgmt.io.outputReady        := ( (currentRouterOutputState(rfResultInt) === VCRouterOutputState.xmit) || (currentRouterOutputState(rfResultInt) === VCRouterOutputState.rdyToXmit))

            // --------------------- 

			creditGen.io.outCredit <> io.inChannels(curInChannel).credit(curInVC)

            // ---- INPUT BUFFER LOGIC ----- 
			val inChannelFlit                       = io.inChannels(curInChannel).flit
			val correctVC                           = inChannelFlit.getVCPort() === UInt(curInVC)

			when (io.inChannels(i).flit.isHead()) {
				headFlitRegFile.io.writeData        := io.inChannels(curInChannel).flit.toBits
				headFlitRegFile.io.writeEnable      := routingInBuffer.io.enq.valid
			} .otherwise {
				headFlitRegFile.io.writeData        := UInt(0)
				headFlitRegFile.io.writeEnable      := Bool(false)
			}
			headFlitRegFile.io.readIncrement        := pop
			routingInBuffer.io.enq.bits             := io.inChannels(curInChannel).flit
			routingInBuffer.io.enq.valid            := io.inChannels(curInChannel).flitValid && correctVC 

            //-------- DEBUG -----------
            val debugStr_1      = "SimpleVCRouter: Flit with VC port" + io.inChannels(curInChannel).flit.getVCPort() + " attempting to write to VC " + j
            val f2fB            = Chisel.Module(new Flit2FlitBundle(parms))
            f2fB.io.inFlit      := Flit.fromBits(io.inChannels(i).flit.toBits, parms)
            val debug_headFlit  = f2fB.io.outHead
            val debug_bodyFlit  = f2fB.io.outBody
            // assert(  (correctVC && io.inChannels(curInChannel).flitValid) || (~io.inChannels(curInChannel).flitValid),                              "SimpleVCRouter:" + routerID + " Incorrect VC found at input " + i + " VC " + j)
            assert( ((~routingInBuffer.io.enq.valid)      || (routingInBuffer.io.enq.valid && correctVC)),          debugStr_1 ) 
            assert( ((routingInBuffer.io.enq.ready        && io.inChannels(curInChannel).flitValid && correctVC) || (~io.inChannels(curInChannel).flitValid) || (~correctVC)),   "SimpleVCRouter:" + routerID +" Insufficent space in input buffer - Credit Ready asserted when Routing In Buffer not ready inputPort= " + i.toString + " curVC = " + j.toString )
			when(flitGranted) {
				io.counters(0).counterVal := UInt(1)
			}.otherwise {
				io.counters(0).counterVal := UInt(0)
			}
            //----- END DEBUG ---------


            // ----- Routing Function Logic -----
			(0 until numOutChannels).map( a => validVCs(index)(a) := routingFunction.io.vcsAvailable(a) )

			routingFunction.io.inHeadFlit := Flit.fromBits(headFlitRegFile.io.readData, parms).asHead()
			// when (headFlitRegFile.io.readValid) && ~flitIsTail) {
				rfResult        := UIntToOH(routingFunction.io.result) 
				rfResultInt     := routingFunction.io.result
			// } .otherwise{
			// 	rfResult        := UInt(0)
			// 	rfResultInt     := UInt(0)
			// }
            // -------------------------------
				lockControl     := pop  
			
            // Head Flit Reg File Pipeline update
			headFlitRegFile.io.wePipelineReg(0)      := (!headFlitRegFile.io.rvPipelineReg(0)) && headFlitRegFile.io.readValid
			headFlitRegFile.io.writePipelineReg(0)   := routingFunction.io.outHeadFlit.toBits

			headFlitRegFile.io.wePipelineReg(1)      := headFlitRegFile.io.rvPipelineReg(0) && vcAllocGranted && (routerStateMgmt.io.currentState === VCRouterState.packetRouted)
			headFlitRegFile.io.writePipelineReg(1)   := headFlitRegFile.io.readPipelineReg(0)

			headFlitRegFile.io.wePipelineReg(2)      := headFlitRegFile.io.rvPipelineReg(1) && swAllocGranted && (routerStateMgmt.io.currentState === VCRouterState.vcAllocGranted)
			headFlitRegFile.io.writePipelineReg(2)   := headFlitRegFile.io.readPipelineReg(1)
            //--------------------------------- 

            // Switch and VC Allocator Logic 
			for (k <- 0 until numOutChannels) {
				swAllocator.io.requests(k)(index).request 	    := rfResult(k).toBool && (routerStateMgmt.io.currentState >= VCRouterState.vcAllocGranted) 
				swAllocator.io.requests(k)(index).releaseLock   := lockControl.toBool && (routerStateMgmt.io.currentState === VCRouterState.swAllocGranted)

				vcAllocator.io.resources(index).ready           := routingInBuffer.io.deq.valid && (routerStateMgmt.io.currentState >= VCRouterState.packetRouted)
	
				swAllocator.io.requests(k)(index).priorityLevel := flitPriority
			
			}
            //--------------------------------- 

            //Routing Input Buffer deque logic
            //Update credits available
			when(routingInBuffer.io.deq.bits.isHead() && ~headFlitRegFile.io.readValid) {
				routingInBuffer.io.deq.ready := UInt(0) 
                creditGen.io.inGrant         := Bool(false)
              //  readyToXmit                  := Bool(false)
			}.elsewhen(routingInBuffer.io.deq.bits.isHead() && headFlitRegFile.io.readValid) {
				routingInBuffer.io.deq.ready        := flitGranted && (routerStateMgmt.io.currentState >= VCRouterState.vcAllocGranted) && creditConsReady(rfResultInt)(assignedVC)
                readyToXmit(index)(rfResultInt)     := flitGranted && (routerStateMgmt.io.currentState >= VCRouterState.vcAllocGranted) && creditConsReady(rfResultInt)(assignedVC) && routingInBuffer.io.deq.valid
                creditGen.io.inGrant                := flitGranted && (routerStateMgmt.io.currentState >= VCRouterState.vcAllocGranted) && creditConsReady(rfResultInt)(assignedVC) && routingInBuffer.io.deq.valid
		//consumeCredit(rfResultInt)(assignedVC) := flitGranted && (routerStateMgmt.io.currentState >= VCRouterState.vcAllocGranted) && creditConsReady(rfResultInt)(assignedVC)
		flitPriority                        := routingInBuffer.io.deq.bits.asHead().priorityLevel
			    
			  }.otherwise{
				routingInBuffer.io.deq.ready        := flitGranted && (routerStateMgmt.io.currentState === VCRouterState.swAllocGranted) && creditConsReady(rfResultInt)(assignedVC)
                readyToXmit(index)(rfResultInt)     := flitGranted && (routerStateMgmt.io.currentState === VCRouterState.swAllocGranted) && creditConsReady(rfResultInt)(assignedVC) && routingInBuffer.io.deq.valid
				creditGen.io.inGrant                := flitGranted && (routerStateMgmt.io.currentState === VCRouterState.swAllocGranted) && creditConsReady(rfResultInt)(assignedVC) && routingInBuffer.io.deq.valid
			    //consumeCredit(rfResultInt)(assignedVC) := flitGranted && (routerStateMgmt.io.currentState === VCRouterState.swAllocGranted) && creditConsReady(rfResultInt)(assignedVC)
			}
			//consumeCredit(rfResultInt)(assignedVC) := routingInBuffer.io.deq.ready
            //--------------------------------- 

            // Update VC in head flit based on allocation 
			val swInputHeadMux =routingInBuffer.io.deq.bits.toBits
			vcReplacer.io.oldFlit := Flit.fromBits(swInputHeadMux, parms)

            when(routerStateMgmt.io.currentState === VCRouterState.packetRouted && vcAllocGranted){
                assignedVC := vcAllocator.io.chosens(index)
            }

            vcReplacer.io.newVCPort := assignedVC
            //--------------------------------- 
            
			// Switch Input Logic
			switch.io.inPorts(index) <> vcReplacer.io.newFlit

		}
	} //for (numIns)

	// Loop through output channels, connect to switch
	for (i <- 0 until numOutChannels) {
		val creditCons = (0 until numVCs).map( a => 
			Chisel.Module ( new CreditCon(parms.child(("CreditCon", a), Map(
				("numCreds"->Soft(routerInCredits))
			))) )
		)

		creditConsReady(i).zipWithIndex.foreach{ case (c,k) =>
			c := creditCons(k).io.outCredit
		}

        val VCRouterOutputState             = new VCRouterOutputState
		val routingOutReg                   = Reg( init=Flit.fromBits(UInt(0), parms) )
		val outCreditMux                    = Chisel.Module( new MuxN[Bool](Bool(), parms.child(("OutCreditMux"), Map(
	                                			("n"->Soft(numVCs))
		                                     ))))


        //VC Allocator controls 
		val vcsAvailForInputs = (0 until numIns).map(validVCs(_)(i))

		for (k <- 0 until numIns) {
			for (j <- 0 until numVCs) {
				var indexOutput : Int = i*numVCs+j

				val releaseVCLockReg                                = Reg(Bool(false))
				releaseVCLockReg                                    := flitsAreTail(k) && creditCons(j).io.outCredit && (routerOutputStateMgmt(i).io.currentState === VCRouterOutputState.xmit) 
				vcAllocator.io.requests(k)(indexOutput).releaseLock := releaseVCLockReg
				vcAllocator.io.requests(k)(indexOutput).request     := vcsAvailForInputs(k)(j)
			}
		}
        //-----------------

        // Switch Allocator Controls
		val outCredits                      = Vec( creditCons.map(_.io.outCredit) )
		val almostOuts						= Vec( creditCons.map(_.io.almostOut) )
		swAllocator.io.resources(i).ready   := Bool(true) 
		var deqVCPort                       = switch.io.outPorts(i).getVCPort() 
        // ------------------- 

        // Output register
		routingOutReg                       := switch.io.outPorts(i)
		io.outChannels(i).flit              := routingOutReg
        // ------------------- 

        //State Machine
        routerOutputStateMgmt(i).io.swAllocGranted  := orR(Vec (readyToXmit.map( a=> a(i))).toBits)  //This should be routed from the input side
        routerOutputStateMgmt(i).io.creditsAvail    := outCredits(deqVCPort) //&& ~almostOuts(deqVCPort)
        // ------------------- 
 
	
		// Credits
		// io.outChannels(i).flitValid := (routerOutputStateMgmt(i).io.currentState >= VCRouterOutputState.rdyToXmit) && ~(routerOutputStateMgmt(i).io.currentState === VCRouterOutputState.hold)
		val flitValidReg = Reg(Bool())
		flitValidReg := routerOutputStateMgmt(i).io.swAllocGranted //consumeCredit(i)
		io.outChannels(i).flitValid := flitValidReg

		creditCons.zipWithIndex.foreach{ case(b,j) =>
			// b.io.inConsume     := (deqVCPort === UInt(j)) && ((routerOutputStateMgmt(i).io.currentState === VCRouterOutputState.rdyToXmit) || (routerOutputStateMgmt(i).io.currentState === VCRouterOutputState.xmit))  //&& outCredits(routingOutReg.getVCPort())
            b.io.inConsume :=    routerOutputStateMgmt(i).io.swAllocGranted &&  (deqVCPort === UInt(j))
		}
		
		creditCons.zipWithIndex.foreach{ case(b, j) => 
			b.io.inCredit.grant := io.outChannels(i).credit(j).grant
		}
	}
}
