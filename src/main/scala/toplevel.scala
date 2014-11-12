package OpenSoC

import Chisel._
import scala.collection.mutable.LinkedHashMap
/*
class OpenSoCParams {
	val NumPorts = UInt()
	// Includes parameters from all child classes
}
*/

class ReadyValid[ T <: Data] (parms : Parameters, tGen : Parameters => T) extends Bundle {

		val packet : T = tGen(parms).asInput
		val packetReady	= Bool(OUTPUT)
		val packetValid	= Bool(INPUT)
}

class OpenSoCChannelPort[T <: Data](parms:Parameters, tGen : Parameters => T) extends Bundle {
	val in = new ReadyValid[T](parms, tGen)
	val out = new Channel(parms).flip()
}

class OpenSoCFlitChannelPort(parms:Parameters) extends Bundle {
	val in = new Channel(parms)
	val out = new Channel(parms).flip()
}

class OpenSoCPacketChannelPort(parms:Parameters) extends Bundle {
	val in = new PacketChannel(parms)
	val out = new Channel(parms).flip()
}


class OpenSoC_CMesh[T<: Data](parms: Parameters, tGen : Parameters => T) extends Module(parms) {
	val Dim = parms.get[Int]("TopologyDimension") // Dimension of topology
	val K = parms.get[Vector[Int]]("RoutersPerDim") // Routers per dimension.
	val C = parms.get[Int]("Concentration") // Processors (endpoints) per router.
	val numRouters : Int = K.product //Math.pow(K, Dim).toInt
	val numPorts = numRouters * C// K.product//(Math.pow(K,Dim)*C).toInt // K^Dim = numRouters, numRouters*C = numPorts
	val queueDepth = parms.get[Int]("queueDepth")
	val numVCs = parms.get[Int]("numVCs")
	val routerRadix = 2 * Dim + C
	val counterMax = UInt(32768)


	val myParms = parms
	
	val io = new Bundle {
		val ports 	= Vec.fill(numPorts) { new OpenSoCChannelPort[T](parms,tGen)}
		val headFlitIn  = new HeadFlit(parms).asInput
		val headFlitOut = new Flit(parms).asOutput
		val bodyFlitIn  = new BodyFlit(parms).asInput
		val bodyFlitOut = new Flit(parms).asOutput
		val headFlitsIn  = Vec.fill(numPorts) { new HeadFlit(parms).asInput }
		val headFlitsOut = Vec.fill(numPorts) { new Flit(parms).asOutput }
		val bodyFlitsIn  = Vec.fill(numPorts) { new BodyFlit(parms).asInput }
		val bodyFlitsOut = Vec.fill(numPorts) { new Flit(parms).asOutput }
		val flitsIn		 = Vec.fill(numPorts) { new Flit(parms).asInput }
		val flitsOutAsHead = Vec.fill(numPorts) { new HeadFlit(parms).asOutput }
		val flitsOutAsBody = Vec.fill(numPorts) { new BodyFlit(parms).asOutput }

		val cyclesRouterBusy	= Vec.fill(numRouters){ UInt(OUTPUT, width=counterMax.getWidth)}
		val cyclesChannelBusy	= Vec.fill(numRouters*routerRadix){UInt(OUTPUT, width=counterMax.getWidth)}
	}


	val flitExtract = Chisel.Module( new Flit2FlitBundle(parms) )
	val headExtract = Chisel.Module( new HeadBundle2Flit(parms) )
	val bodyExtract = Chisel.Module( new BodyBundle2Flit(parms) )

	io.headFlitIn 	<> headExtract.io.inHead
	io.headFlitOut  <> headExtract.io.outFlit
	io.bodyFlitIn 	<> bodyExtract.io.inBody
	io.bodyFlitOut  <> bodyExtract.io.outFlit


	for (port <- 0 until numPorts){
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
	
	println("numVCs: " + numVCs)
	
	if (numVCs < 2) {
		println("In Wormhole Mode")
		println("------------------")
		val topology = Chisel.Module(
			new CMesh(parms.child("CMeshTopo", Map(
				("topoInCredits"->Soft(queueDepth)),
				("topoOutCredits"->Soft(queueDepth)),

				("rfCtor"->Soft((parms: Parameters) => new CMeshDOR(parms))),
				("routerCtor"->Soft((parms: Parameters) => new SimpleRouter(parms)))
		))))

		for (i <- 0 until numPorts) {
			val injectionQ = Chisel.Module( new GenericChannelQ(parms.child(("InjectionQ", i)))  )
			val ejectionQ = Chisel.Module( new GenericChannelQ(parms.child(("EjectionQ", i))) )
			val inputNetIface = Chisel.Module( new InputPacketInterface[T](parms.child(("inputNetIface", i)),tGen) )

			inputNetIface.io.in.bits := io.ports(i).in.packet 
			inputNetIface.io.in.valid := io.ports(i).in.packetValid
			io.ports(i).in.packetReady := inputNetIface.io.in.ready
		
			injectionQ.io.in <> inputNetIface.io.out

			topology.io.inChannels(i) <> injectionQ.io.out
			ejectionQ.io.in <> topology.io.outChannels(i)
			io.ports(i).out <> ejectionQ.io.out
		}
	} else {	
		println("In VC Mode")
		println("------------------")
		val topology = Chisel.Module(
			new VCCMesh(parms.child("VCCMeshTopo", Map(
				("topoInCredits"->Soft(queueDepth)),
				("topoOutCredits"->Soft(queueDepth)),
			
				("rfCtor"->Soft((parms: Parameters) => new CMeshDOR(parms))),
				("routerCtor"->Soft((parms: Parameters) => new SimpleVCRouter(parms)))
		))))

		for (i <- 0 until numPorts) {
			val injectionQ = Chisel.Module( new InjectionChannelQ(parms.child(("InjectionQ", i), Map(
				("vcArbCtor"->Soft((parms: Parameters) => new RRArbiter(parms)))
			)) ) )
			val ejectionQ = Chisel.Module( new EjectionChannelQ(parms.child(("EjectionQ", i))) )
			val inputNetIface = Chisel.Module( new InputPacketInterface[T](parms.child(("inputNetIface", i)),tGen) )

			inputNetIface.io.in.bits := io.ports(i).in.packet 
			inputNetIface.io.in.valid := io.ports(i).in.packetValid
			io.ports(i).in.packetReady := inputNetIface.io.in.ready

			injectionQ.io.in <> inputNetIface.io.out

			// injectionQ.io.in.packet := io.ports(i).in.packet 
			// injectionQ.io.in.packetValid := io.ports(i).in.packetValid
			// io.ports(i).in.packetReady := injectionQ.io.in.packetReady
			topology.io.inChannels(i) <> injectionQ.io.out
			ejectionQ.io.in <> topology.io.outChannels(i)
			io.ports(i).out <> ejectionQ.io.out
		}
	}
//	for (r <- 0 until numRouters) {
//		io.cyclesRouterBusy(r) := topology.io.cyclesRouterBusy(r)
///		for(c <- 0 until routerRadix) {
//			io.cyclesChannelBusy((r*routerRadix) + c) := topology.io.cyclesChannelBusy((r*routerRadix) + c)
//		}
//	}
}

class OpenSoC_FlitChannel(parms : Parameters) extends Bundle{
	val flitIsHead 	= Chisel.Bool()
	val headFlit	= new HeadFlit(parms)
	val bodyFlit	= new BodyFlit(parms)
}

class OpenSoC_NetworkChannel(parms : Parameters) extends Bundle {
	val in 			= new OpenSoC_FlitChannel(parms).asInput
	val inCredit 	= new Credit().asInput
	val out			= new OpenSoC_FlitChannel(parms).asOutput
	val outCredit	= new Credit().asOutput
}

class OpenSoC_CFlatBfly[T<: Data](parms: Parameters, tGen : Parameters => T) extends Module(parms) {
	val Dim = parms.get[Int]("TopologyDimension") // Dimension of topology
	val K = parms.get[Vector[Int]]("RoutersPerDim") // Routers per dimension.
	val C = parms.get[Int]("Concentration") // Processors (endpoints) per router.
	val numRouters : Int = K.product //Math.pow(K, Dim).toInt
	val numPorts = numRouters * C// K.product//(Math.pow(K,Dim)*C).toInt // K^Dim = numRouters, numRouters*C = numPorts
	val queueDepth = parms.get[Int]("queueDepth")
	val numVCs = parms.get[Int]("numVCs")
	val routerRadix 		= K.sum - Dim + C
	val counterMax = UInt(32768)
	
	val io = new Bundle {
		val ports = Vec.fill(numPorts) { new OpenSoCChannelPort[T](parms, tGen) }
		val headFlitIn  = new HeadFlit(parms).asInput
		val headFlitOut = new Flit(parms).asOutput
		val bodyFlitIn  = new BodyFlit(parms).asInput
		val bodyFlitOut = new Flit(parms).asOutput
		val headFlitsIn  = Vec.fill(numPorts) { new HeadFlit(parms).asInput }
		val headFlitsOut = Vec.fill(numPorts) { new Flit(parms).asOutput }
		val bodyFlitsIn  = Vec.fill(numPorts) { new BodyFlit(parms).asInput }
		val bodyFlitsOut = Vec.fill(numPorts) { new Flit(parms).asOutput }
		val flitsIn		 = Vec.fill(numPorts) { new Flit(parms).asInput }
		val flitsOutAsHead = Vec.fill(numPorts) { new HeadFlit(parms).asOutput }
		val flitsOutAsBody = Vec.fill(numPorts) { new BodyFlit(parms).asOutput }

		val cyclesRouterBusy	= Vec.fill(numRouters){ UInt(OUTPUT, width=counterMax.getWidth)}
		val cyclesChannelBusy	= Vec.fill(numRouters*routerRadix){UInt(OUTPUT, width=counterMax.getWidth)}
	}

	val flitExtract = Chisel.Module( new Flit2FlitBundle(parms) )
	val headExtract = Chisel.Module( new HeadBundle2Flit(parms) )
	val bodyExtract = Chisel.Module( new BodyBundle2Flit(parms) )

	io.headFlitIn 	<> headExtract.io.inHead
	io.headFlitOut  <> headExtract.io.outFlit
	io.bodyFlitIn 	<> bodyExtract.io.inBody
	io.bodyFlitOut  <> bodyExtract.io.outFlit

	for (port <- 0 until numPorts){
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

	if (numVCs > 1) {
		println("In VC Mode")
		println("------------------")
		val topology = Chisel.Module(
			new VCCFlatBfly(parms.child("VCCFlatBflyTopo", Map(
				("topoInCredits"->Soft(queueDepth)),
				("topoOutCredits"->Soft(queueDepth)),

				("rfCtor"->Soft((parms: Parameters) => new CFlatBflyDOR(parms))),
				("routerCtor"->Soft((parms: Parameters) => new SimpleVCRouter(parms)))
			))))

		for (i <- 0 until numPorts) {
			val injectionQ = Chisel.Module( new InjectionChannelQ(parms.child(("InjectionQ", i), Map(
				("vcArbCtor"->Soft((parms: Parameters) => new RRArbiter(parms)))
			)) ) )
			val ejectionQ = Chisel.Module( new EjectionChannelQ(parms.child(("EjectionQ", i))) )
			val inputNetIface = Chisel.Module( new InputPacketInterface[T](parms.child(("inputNetIface", i)),tGen) )

			inputNetIface.io.in.bits := io.ports(i).in.packet 
			inputNetIface.io.in.valid := io.ports(i).in.packetValid
			io.ports(i).in.packetReady := inputNetIface.io.in.ready

			injectionQ.io.in <> inputNetIface.io.out

			println("connecting port " + i + " to injection Q ")
			topology.io.inChannels(i) <> injectionQ.io.out
			ejectionQ.io.in <> topology.io.outChannels(i)
			io.ports(i).out <> ejectionQ.io.out
		}
	} else {
		println("In Wormhole Mode")
		println("------------------")
		val topology = Chisel.Module(
			new CFlatBfly(parms.child("CFlatBflyTopo", Map(
				("topoInCredits"->Soft(queueDepth)),
				("topoOutCredits"->Soft(queueDepth)),

				("rfCtor"->Soft((parms: Parameters) => new CFlatBflyDOR(parms))),
				("routerCtor"->Soft((parms: Parameters) => new SimpleRouter(parms)))
			))))

		for (i <- 0 until numPorts) {
			val injectionQ = Chisel.Module( new GenericChannelQ(parms.child(("InjectionQ", i)))  )
			val ejectionQ = Chisel.Module( new GenericChannelQ(parms.child(("EjectionQ", i))) )
			val inputNetIface = Chisel.Module( new InputPacketInterface[T](parms.child(("inputNetIface", i)),tGen) )

			inputNetIface.io.in.bits := io.ports(i).in.packet 
			inputNetIface.io.in.valid := io.ports(i).in.packetValid
			io.ports(i).in.packetReady := inputNetIface.io.in.ready

			injectionQ.io.in <> inputNetIface.io.out

			println("connecting port " + i + " to injection Q ")
			topology.io.inChannels(i) <> injectionQ.io.out
			ejectionQ.io.in <> topology.io.outChannels(i)
			io.ports(i).out <> ejectionQ.io.out
		}
	}

	// for (r <- 0 until numRouters) {
	// 	io.cyclesRouterBusy(r) := topology.io.cyclesRouterBusy(r)
	// 	for(c <- 0 until routerRadix) {
	// 		io.cyclesChannelBusy((r*routerRadix) + c) := topology.io.cyclesChannelBusy((r*routerRadix) + c)
	// 	}
	// }
}

/*class OpenSoC_CFlatBflyTester(c: OpenSoC_CFlatBfly) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	expect(c.io.ports(0).in.credit.valid, 1)
}*/

class Flit2FlitBundle(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val inFlit = new Flit(parms).asInput
		val outHead = new HeadFlit(parms).asOutput
		val outBody = new BodyFlit(parms).asOutput
	}
	io.outHead := io.inFlit.asHead()
	io.outBody := io.inFlit.asBody()
}

class HeadBundle2Flit(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val inHead = new HeadFlit(parms).asInput
		val outFlit = new Flit(parms).asOutput
	}
	io.outFlit := Flit.head(io.inHead)
}

class BodyBundle2Flit(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val inBody = new BodyFlit(parms).asInput
		val outFlit = new Flit(parms).asOutput
	}
	io.outFlit := Flit.body(io.inBody)
}
