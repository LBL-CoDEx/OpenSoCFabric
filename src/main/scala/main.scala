package OpenSoC

import Chisel._
	
object OpenSoC {
    def main(args: Array[String]): Unit = {
		val mySWargs = Array("--backend", "c", "--genHarness", "--compile", "--parallelMakeJobs", "-1", "--compileInitializationUnoptimized", "--lineLimitFunctions", "1024", "--minimumLinesPerFile", "32768", "--test", "--Wall", "--vcd", "--reportDims")//, "--debug", "--ioDebug")
		val myHWargs = Array("--backend", "v", "--genHarness", "--vcd")
		var parms = Parameters.empty

		val Dim = 2 // Dimension of topology
		val K = Vector(2, 2) // Routers per dimension.
		val C = 1 // Processors (endpoints) per router.
		val numVCs = 1 // Number of Virtual Channels
		
		val numPortsCMesh = Dim*2+C
        var injRate :Double = 1.0
		val numPortsCFlatBfly = K.sum - Dim + C
		val numPorts = numPortsCMesh
		var harnessName : String = ""
		var moduleName  : String = ""
        var myargs : Array[String] = Array() 
		var moduleToTest : () => Chisel.Module = () => Chisel.Module(new MuxN[UInt](UInt(width=32),
													parms.child("MyMux", Map(("n"->Soft(4))))
													))
		args.sliding(2).foreach(arg =>
			arg(0) match {
				case "--harnessName"    => (harnessName = arg(1))
				case "--moduleName"     => (moduleName = arg(1))
                case "--injRate"        => (injRate = arg(1).toDouble)
                case "--sw"             => (myargs = mySWargs)
                case "--hw"             => (myargs = myHWargs) 
				case _ => Nil
			}
		)
		printf("Harness: %s Module: %s\n", harnessName, moduleName)

		def MakePacketChannel (parms: Parameters) : PacketChannel = {
			val channel = new PacketChannel(parms)
			channel
		}
	
		moduleName match {	
			case "RingBuffer"	=>	(moduleToTest = () => Module(new RingBuffer(
													parms.child("MyRingBuffer", Map(
														("widthRingBuffer"->Soft(32)),
														("pointerCount"->Soft(3)),
														("totalRingBufferEntries"->Soft(16))
														)
													))	
												))
			case "MuxN"			=>	(moduleToTest = () => Module(new MuxN[UInt](UInt(width=32),
													parms.child("MyMux", Map(
														("n"->Soft(4))
														)
													))
												))
			case "Switch"		=>	(moduleToTest = ()  => Module(new Switch[UInt](UInt(width=32),
													parms.child("MySwitch", Map(
														("numInPorts"->Soft(8)),
														("numOutPorts"->Soft(3))
														)
													))
												))
			case "RRArbiter"	=>	(moduleToTest = () =>  Module(new RRArbiter(
													parms.child("MyRRArbiter", Map(
														("numReqs"->Soft(8))
														)
													))
												))
			case "SwitchAllocator"	=>	(moduleToTest = () => Module(new SwitchAllocator(
													parms.child("MyAllocator", Map(
														("numReqs"->Soft(4)),
														("numRes"->Soft(3)),
														("arbCtor"->Hard( (parms: Parameters) => new RRArbiter(parms) ))
														)
													))
												))
			case "RouterRegFile"	=>	(moduleToTest = () => Module(new RouterRegFile(
													parms.child("MyRouterRegFile", Map(
														("widthRegFile"->Soft(32)),
														("depthRegFile"->Soft(16)),
														("pipelineDepth"->Soft(1))
														)
													))
												))
			case "GenericChannelQ"	=>	(moduleToTest = () => Module(new GenericChannelQ(
													parms.child("MyChannelQ", Map(
														("queueDepth"->Soft(16)),
		
														("flitWidth"->Hard(32)),
														("packetIDWidth"->Hard(4)),
			
														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(2)),
														("destCordDim"->Hard(3)),

														("flitIDWidth"->Hard(1)),
														("numInputVCs"->Hard(2)),
														("payloadWidth"->Hard(1))
														)
													))
												))
			case "PacketInjectionQ"	=>	(moduleToTest = () => Module(new PacketInjectionQ[Packet](
													parms.child("MyChannelQ", Map(
														("queueDepth"->Soft(16)),
		
														("flitWidth"->Hard(32)),
														("packetMaxLength"->Hard(16)),
														("packetWidth"->Hard(32)),
														("packetIDWidth"->Hard(4)),
			
														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(2)),
														("destCordDim"->Hard(3)),

														("flitIDWidth"->Hard(1)),
														("numInputVCs"->Hard(2)),
														("numVCs"->Soft(numVCs)),
														("payloadWidth"->Hard(1)),
														("Dim"->Hard(Dim))
														)
													), (p)=>new Packet(p))
												))
			case "VCIEChannelQ" =>	(moduleToTest = () => Module(new VCIEChannelQ(
													parms.child("MyVCChannelQs", Map(
														("queueDepth"->Soft(4)),

														("vcArbCtor"->Hard( (parms: Parameters) => new RRArbiter(parms) )),
														("numVCs"->Hard(numVCs)),
		
														("packetIDWidth"->Hard(4)),
			
														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(2)),
														("destCordDim"->Hard(3)),

														("flitIDWidth"->Hard(1)),
														("payloadWidth"->Hard(1))
														)
													))
												))
			case "InjectionChannelQ" =>	(moduleToTest = () => Module(new InjectionChannelQ(
													parms.child("MyChannelIQ", Map(
														("queueDepth"->Soft(4)),

														("vcArbCtor"->Hard( (parms: Parameters) => new RRArbiter(parms) )),
														("numVCs"->Hard(numVCs)),
		
														("packetIDWidth"->Hard(4)),
			
														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(2)),
														("destCordDim"->Hard(3)),

														("flitIDWidth"->Hard(1)),
														("payloadWidth"->Hard(1))
														)
													))
												))
			case "PacketToFlit"	=>	(moduleToTest = () => Module(new PacketToFlit(
													parms.child("MyPacketToFlit", Map(
		
														("packetMaxLength"->Hard(16)),
														("packetWidth"->Hard(32)),
														("packetIDWidth"->Hard(4)),
														("payloadWidth"->Hard(32)),
														("flitIDWidth"->Hard(4)),
			
														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(2)),
														("destCordDim"->Hard(3)),
														("numVCs"->Hard(numVCs)),
														("numInputVCs"->Hard(2)),
														("Dim"->Hard(Dim))

														)
													))
												))
			case "BusProbe"	=>	(moduleToTest = () => Module(new BusProbe(
													parms.child("BusProbe", Map(
		
														("packetMaxLength"->Hard(16)),
														("TopologyDimension"->Hard(Dim)),
														("RoutersPerDim"->Hard(K)),
														("Concentration"->Hard(C)),
														("packetWidth"->Hard(32)),
														("packetIDWidth"->Hard(4)),
														("payloadWidth"->Hard(32)),
														("flitIDWidth"->Hard(4)),
			
														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(2)),
														("destCordDim"->Hard(3)),
														("numVCs"->Hard(numVCs)),
														("numInputVCs"->Hard(2))

														)
													))
												))
			case "CreditTester"	=> (moduleToTest = () => Module(new CreditTester(
													parms.child("MyCreditTester", Map(
														("numCreds"->Hard(4))
														)
													))
												))
			case "CMeshDOR"		=> (moduleToTest = () => Module(new CMeshDOR(
													parms.child("MyCMDORTester", Map(
														("TopologyDimension"->Hard(Dim)),
														("RoutersPerDim"->Hard(K)),
														("Concentration"->Hard(C)),

														("routingCoord"->Hard(Vector(0,0))),
														("numResources"->Hard(numPortsCMesh)),

														("packetIDWidth"->Hard(4)),
														("numVCs"->Hard(numVCs)),

														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(Math.max(log2Up(K.max),log2Up(C)))),
														("destCordDim"->Hard(Dim + 1))
														)
													))
												))
			case "CFlatBflyDOR"	=> (moduleToTest = () => Module(new CFlatBflyDOR(
													parms.child("MyCFBDORTester", Map(
														("TopologyDimension"->Hard(Dim)),
														("RoutersPerDim"->Hard(K)),
														("Concentration"->Hard(C)),

														("routingCoord"->Hard(Vector(5,3))),
														("numResources"->Hard(numPortsCFlatBfly)),

														("packetIDWidth"->Hard(4)),
														("numVCs"->Hard(numVCs)),

														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(Math.max(log2Up(K.max),log2Up(C)))),
														("destCordDim"->Hard(Dim + 1))
														)
													))
												))
			case "SimpleRouterTestWrapper"	=> (moduleToTest = () => Module(new SimpleRouterTestWrapper(
													parms.child("MySimpleRouter", Map(
														("TopologyDimension"->Hard(Dim)),
														("RoutersPerDim"->Hard(K)),
														("Concentration"->Hard(C)),

														("numInChannels"->Soft(numPorts)),
														("numOutChannels"->Soft(numPorts)),
														("queueDepth"->Soft(9)),
														("routerInCredits"->Soft(8)),
														("routerOutCredits"->Soft(8)),
														("numInputVCs"->Soft(1)),
														("numOutputVCs"->Soft(1)),
														("numVCs"->Soft(numVCs)),

														("routerID"->Hard(Vector(0,0))),
														("rfCtor"->Soft((parms: Parameters) => new CMeshDOR(parms))),

														("packetIDWidth"->Hard(8)),

														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(Math.max(log2Up(K.max),log2Up(C)))),
														("destCordDim"->Hard(Dim + 1)),
														("routerCtor"->Soft((parms: Parameters) => new SimpleRouter(parms))),

														("flitIDWidth"->Hard(4)),
														("payloadWidth"->Hard(16))
														)
													))
												))
			case "OpenSoC_CMesh"	=> (moduleToTest = () => Module(new OpenSoC_CMesh[Packet](
													parms.child("MyOpenSoC_CMesh", Map(
														("TopologyDimension"->Hard(Dim)),
														("RoutersPerDim"->Hard(K)),
														("Concentration"->Hard(C)),
														("numInputVCs"->Soft(1)),
														("numOutputVCs"->Soft(1)),
														("numVCs"->Soft(numVCs)),

														("queueDepth"->Soft(16)),

														("packetIDWidth"->Hard(8)),
														("packetMaxLength"->Hard(16)),
														("packetWidth"->Hard(32)),

														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(Math.max(log2Up(K.max),log2Up(C)))),
														("destCordDim"->Hard(Dim + C)),

														("flitIDWidth"->Hard(4)),
														("payloadWidth"->Hard(32)),
														("InputFlitizer"->Soft((parms: Parameters) => new PacketToFlit(parms)))
														)
													), (p)=>new Packet(p))
												))
			case "OpenSoC_CMesh_Flit"	=> (moduleToTest = () => Module(new OpenSoC_CMesh[Flit](
													parms.child("MyOpenSoC_CMesh", Map(
														("TopologyDimension"->Hard(Dim)),
														("RoutersPerDim"->Hard(K)),
														("Concentration"->Hard(C)),
														("numVCs"->Soft(numVCs)),

														("queueDepth"->Soft(16)),

														("packetIDWidth"->Hard(16)),
														("packetMaxLength"->Hard(16)),
														("packetWidth"->Hard(32)),

														("packetTypeWidth"->Hard(4)),
														("destCordWidth"->Hard(Math.max(log2Up(K.max),log2Up(C)))),
														("destCordDim"->Hard(Dim + C)),

														("flitIDWidth"->Hard(4)),
														("payloadWidth"->Hard(32)),
														("Dim"->Hard(Dim)),
														("InputFlitizer"->Soft((parms: Parameters) => new FlitToFlit(parms)))
														)
													), (p)=>new Flit(p))
												))
			case "OpenSoC_CFlatBfly"	=> (moduleToTest = () => Module(new OpenSoC_CFlatBfly[Flit](
													parms.child("MyOpenSoC_CFlatBfly", Map(
														("TopologyDimension"->Hard(Dim)),
														("RoutersPerDim"->Hard(K)),
														("Concentration"->Hard(C)),
														("numResources"->Hard(numPortsCFlatBfly)),

														("queueDepth"->Soft(16)),
														("numVCs"->Soft(numVCs)),
														("numOutputVCs"->Soft(1)),
		
														("packetTypeWidth"->Hard(4)),
														("packetMaxLength"->Hard(16)),
														("packetWidth"->Hard(32)),
														("packetIDWidth"->Hard(8)),
														("destCordWidth"->Hard(Math.max(log2Up(K.max),log2Up(C)))),
														("destCordDim"->Hard(Dim + C)),

														("flitIDWidth"->Hard(4)),
														("payloadWidth"->Hard(32)),
														("InputFlitizer"->Soft((parms: Parameters) => new FlitToFlit(parms)))
														)
													), (p)=>new Flit(p) )
												))
			case _	=> (printf("Unknown Module Name: %s\n", moduleName) )
		}

		harnessName match {

			case "RingBufferTest"			=> ( chiselMainTest(myargs, moduleToTest) { c => new RingBufferTest(c.asInstanceOf[RingBuffer]) } )
			case "MuxNTest"					=> ( chiselMainTest(myargs, moduleToTest) { c => new MuxNTest(c.asInstanceOf[MuxN[UInt]]) } )
			case "SwitchTest"					=> ( chiselMainTest(myargs, moduleToTest) { c => new SwitchTest(c.asInstanceOf[Switch[UInt]]) } )
			case "RRArbiterTest"			=> ( chiselMainTest(myargs, moduleToTest) { c => new RRArbiterTest(c.asInstanceOf[RRArbiter]) } )
			case "SwitchAllocTest"			=> ( chiselMainTest(myargs, moduleToTest) { c => new SwitchAllocTest(c.asInstanceOf[SwitchAllocator]) } )
			case "RouterRegFileTest"		=> ( chiselMainTest(myargs, moduleToTest) { c => new RouterRegFileTest(c.asInstanceOf[RouterRegFile]) } )
			case "PacketToFlitTest"			=> ( chiselMainTest(myargs, moduleToTest) { c => new PacketToFlitTest(c.asInstanceOf[PacketToFlit]) } )
			case "BusProbeTest"				=> ( chiselMainTest(myargs, moduleToTest) { c => new BusProbeTest(c.asInstanceOf[BusProbe]) } )
			case "ChannelQTest"				=> ( chiselMainTest(myargs, moduleToTest) { c => new ChannelQTest(c.asInstanceOf[GenericChannelQ]) } )
			case "PacketInjectionQTest"		=> ( chiselMainTest(myargs, moduleToTest) { c => new PacketInjectionQTest(c.asInstanceOf[PacketInjectionQ[Packet]]) } )
			case "VCChannelQTest"			=> ( chiselMainTest(myargs, moduleToTest) { c => new VCChannelQTest(c.asInstanceOf[VCIEChannelQ]) } )
			case "VCIQTest"					=> ( chiselMainTest(myargs, moduleToTest) { c => new VCIQTest(c.asInstanceOf[InjectionChannelQ]) } )
			case "WHCreditTest"				=> ( chiselMainTest(myargs, moduleToTest) { c => new WHCreditTest(c.asInstanceOf[CreditTester]) } )
			case "CMDORTester"				=> ( chiselMainTest(myargs, moduleToTest) { c => new CMDORTester(c.asInstanceOf[CMeshDOR]) } )
			case "CFlatBflyDORTester"		=> ( chiselMainTest(myargs, moduleToTest) { c => new CFlatBflyDORTester(c.asInstanceOf[CFlatBflyDOR]) } )
			case "SimpleRouterTester"		=> ( chiselMainTest(myargs, moduleToTest) { c => new SimpleRouterTester(c.asInstanceOf[SimpleRouterTestWrapper]) } )
			// case "RouterRandomTester"		=> ( chiselMainTest(myargs, moduleToTest) { c => new RouterRandomTester(c.asInstanceOf[SimpleRouterTestWrapper], parms) } )
		//	case "OpenSoC_CFlatBflyTester"		=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CFlatBflyTester(c.asInstanceOf[OpenSoC_CFlatBfly]) } )
			case "OpenSoC_CFlatBtflyTester_Random_C2"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CFlatBtflyTester_Random_C2(c.asInstanceOf[OpenSoC_CFlatBfly[Flit]], parms) } )
			case "OpenSoC_CFlatBtflyTester_Random"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CFlatBtflyTester_Random(c.asInstanceOf[OpenSoC_CFlatBfly[Flit]], parms) } )
			//case "OpenSoC_CMeshTester_2"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CMeshTester_2(c.asInstanceOf[OpenSoC_CMesh]) } )
			//case "OpenSoC_CFlatBflyTester"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CFlatBflyTester(c.asInstanceOf[OpenSoC_CFlatBfly]) } )
			case "OpenSoC_CMeshTester_Random"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CMeshTester_Random(c.asInstanceOf[OpenSoC_CMesh[Flit]], parms) } )
            case "OpenSoC_CMeshTester_Random_VarInjRate" => ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CMeshTester_Random_VarInjRate(c.asInstanceOf[OpenSoC_CMesh[Flit]], parms, injRate) } )
			case "OpenSoC_CMeshTester_Random_C2"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CMeshTester_Random_C2(c.asInstanceOf[OpenSoC_CMesh[Flit]], parms) } )
			case "OpenSoC_CMeshTester_Random_Packet"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CMeshTester_Combined_Packet(c.asInstanceOf[OpenSoC_CMesh[Packet]], parms, "Random") } )
			//case "OpenSoC_CMeshTester_Neighbor"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CMeshTester_Neighbor(c.asInstanceOf[OpenSoC_CMesh], parms) } )
			case "OpenSoC_CMeshTester_Neighbor_Packet"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CMeshTester_Combined_Packet(c.asInstanceOf[OpenSoC_CMesh[Packet]], parms, "Neighbor") } )
			case "OpenSoC_CMeshTester_BitReverse_Packet"	=> ( chiselMainTest(myargs, moduleToTest) { c => new OpenSoC_CMeshTester_Combined_Packet(c.asInstanceOf[OpenSoC_CMesh[Packet]], parms, "BitReverse") } )
			case _							=> (printf(" Unknown Test Harness Name: %s\n", harnessName))
		}
			
	} //END chiselMain()
/*
    var PacketIDs = UInt(1)
    // This hashmap is indexed by destination coordinates, packet id, and then flit id. It returns the flit that is expected for that tuple.
    var flittable = new scala.collection.mutable.HashMap[(Vector[Int],Int,Int),Flit]

    // This function checks if the flit that was ejected at a specific destination (each destination should know its coordinate vector) was expected and is correct.
    //def TestFlit(dest:Vector[UInt],incomingflit:Flit): Boolean = {
	//assert(flittable.contains((dest,incomingflit.PacketID,incomingflit.FlitID))
        //flit match {
	//	case body: BodyFlit => body // Test that bodyflit specific fields match
	//	case head: HeadFlit => head // Test headflit specific fields
	//}

    //}

    // NOTE: The below assumes a parameters object named: parms

    // Creates a packet of the given size, to the given destination, and of the given type. Payload is random bits. 
    def CreatePacket(dest:Vector[UInt], packettype:OpenSoC.PacketEnum, packetsize:Int): Vector[Flit] = { // "packetsize" is in number of flits
	var flitsofpacket = Array[Flit](packetsize)
	val randomnumber = new scala.util.Random
	for (i <- 0 until packetsize) {
		if (i == 0) { // The tirst flit is a head flit.
			var newflit = new HeadFlit
			newflit.PacketID := PacketIDs
			newflit.FlitID := UInt(i)
			newflit.IsTail := Bool(i == packetsize - 1)
			newflit.RoutingMode := UInt(0) // Unused for now
			newflit.PacketType := packettype
			newflit.Destination := dest
			flitsofpacket(i) = newflit
		}
		else { 
			var newflit = new BodyFlit
			newflit.PacketID := PacketIDs
			newflit.FlitID := UInt(i)
			newflit.IsTail := Bool(i == packetsize - 1)
			newflit.PayLoad := UInt(randomnumber.nextInt())
			flitsofpacket(i) = newflit
		}
		assert(flittable.contains(dest, PacketIDs, i) == Boolean(false))
		flittable((dest, PacketIDs, i)) = newflit
	}
	PacketIDs += UInt(1)
	flitsofpacket.toVector
    }

    // Creates a packet to a random destination, of a random type, and of random size
    def CreateRandomPacket(maxflits:UInt): Vector[Flit] = {
	val randomnumber = new scala.util.Random
	val sizeinflits = randomnumber.nextInt(maxflits + 1)
	val packettype = 0 // Unused for now
	var destcoords = new Array[UInt](Dim + 1)
	for (i <- 0 until Dim) {
		destcoords(i) = UInt(randomnumber.nextInt(K + 1)) // This needs to change slightly when we support non-square topologies (that have a vector of Ks).
	}
	destcoords(Dim) = UInt(randomnumber.nextInt(C)) // Choose the processor port (ejection channel) in the destination router.
	CreatePacket(destcoords.toVector, packettype, sizeinflits)
    }
*/
}
