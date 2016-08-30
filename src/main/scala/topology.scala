package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random
import scala.runtime.ScalaRunTime._

/*abstract class TopologyParams {
	val NumInPorts = UInt()
	val NumOutPorts = UInt()
	val RoutingFuncID = UInt()
	val RouterType = RouterClass
	// Topology Specific paramters are defined in child instantiations of this parameters class
}*/

class CreditBuffer(parms: Parameters) extends Module(parms){

    val numVCs = parms.get[Int]("numVCs")
	val Dim = parms.get[Int]("TopologyDimension") // Dimension of topology
	val C = parms.get[Int]("Concentration") // Processors (endpoints) per router.
    
    val routerRadix = 2 * Dim + C
    
    val io = new Bundle{
        val in  = Vec.fill(routerRadix) { new ChannelVC(parms) }
        val out = Vec.fill(routerRadix) { new ChannelVC(parms).flip() }
    }

    for (i <- 0 until routerRadix){
        io.out(i).flit         := io.in(i).flit
        io.out(i).flitValid    := io.in(i).flitValid
    
        for (c <- 0 until numVCs){ 
            val creditDelay = Reg(init=UInt(0, width=1 ))
            creditDelay := io.out(i).credit(c).grant
            io.in(i).credit(c).grant := creditDelay
        }
    }

}



abstract class Topology(parms: Parameters) extends Module(parms) {
	val Dim = parms.get[Int]("TopologyDimension") // Dimension of topology
	val K = parms.get[Vector[Int]]("RoutersPerDim") // Routers per dimension.
	val C = parms.get[Int]("Concentration") // Processors (endpoints) per router.
	val numVCs = parms.get[Int]("numVCs")
	
	val numRouters : Int = K.product //Math.pow(K, Dim).toInt
	val numIOChannels = numRouters * C // Channels from Proc to Router
	
	val topoInCredits 	= parms.get[Int]("topoInCredits")
	val topoOutCredits 	= parms.get[Int]("topoOutCredits")
	val routingFuncCtor 	= parms.get[Parameters=>RoutingFunction]("rfCtor")
	val routerCtor 		= parms.get[Parameters=>Router]("routerCtor")
	
	val counterMax = UInt(32768)
	
	val io = new Bundle {
		val inChannels  		= Vec.fill(numIOChannels) { new Channel(parms) }
		val outChannels 		= Vec.fill(numIOChannels) { new Channel(parms).flip() }
		//	val cyclesRouterBusy	= Vec.fill(numRouters){ UInt(OUTPUT, width=counterMax.getWidth)}
		//	val cyclesChannelBusy	= Vec.fill(numRouters*routerRadix){UInt(OUTPUT, width=counterMax.getWidth)}
		val cyclesRouterBusy	= Vec.fill(128){ UInt(OUTPUT, width=counterMax.getWidth)}	//DDD: Hack! need to create a counter per router, not some magic number
		val cyclesChannelBusy	= Vec.fill(128*5){UInt(OUTPUT, width=counterMax.getWidth)}
	}
}

abstract class VCTopology(parms: Parameters) extends Module(parms) {
	val Dim = parms.get[Int]("TopologyDimension") // Dimension of topology
	val K = parms.get[Vector[Int]]("RoutersPerDim") // Routers per dimension.
	val C = parms.get[Int]("Concentration") // Processors (endpoints) per router.
	val numVCs = parms.get[Int]("numVCs")
	
	val numRouters : Int = K.product //Math.pow(K, Dim).toInt
	val numIOChannels = numRouters * C // Channels from Proc to Router
	
	val topoInCredits 	= parms.get[Int]("topoInCredits")
	val topoOutCredits 	= parms.get[Int]("topoOutCredits")
	val routingFuncCtor 	= parms.get[Parameters=>RoutingFunction]("rfCtor")
	val routerCtor 		= parms.get[Parameters=>VCRouter]("routerCtor")
	
	val counterMax = UInt(32768)
	
	val io = new Bundle {
		val inChannels  		= Vec.fill(numIOChannels) { new ChannelVC(parms) }
		val outChannels 		= Vec.fill(numIOChannels) { new ChannelVC(parms).flip() }
		//	val cyclesRouterBusy	= Vec.fill(numRouters){ UInt(OUTPUT, width=counterMax.getWidth)}
		//	val cyclesChannelBusy	= Vec.fill(numRouters*routerRadix){UInt(OUTPUT, width=counterMax.getWidth)}
		val cyclesRouterBusy	= Vec.fill(128){ UInt(OUTPUT, width=counterMax.getWidth)}	//DDD: Hack! need to create a counter per router, not some magic number
		val cyclesChannelBusy	= Vec.fill(128*5){UInt(OUTPUT, width=counterMax.getWidth)}
	}
}

	// This class instantiates the proper number of routers and channels
	// It then connects them (by calling member functions?) according to the specific topology

// This topology constructs a multi-dimensional mesh that has the same number of routers in each dimension.
// It is future work to lift this restriction, which will require a tuple or vector instead of the "RoutersPerDim" parameter.
class CMesh(parms: Parameters) extends Topology(parms) {
	
	// val numRouters : Int = K.product //Math.pow(K, Dim).toInt
	val routerRadix = 2 * Dim + C
	val numChannels = numRouters * 2 * Dim  // Channels from Router to Router
	// val numIOChannels = numRouters * C // Channels from Proc to Router


	// Make sure the parameters have been given correctly.
	//assert(numRouters * C == InChannels && numRouters * C == OutChannels)

	var routermap = new HashMap[Vector[Int], Router]()
	//var channelmap = new HashMap[Any,Any]()
	var coord = Vector.fill(Dim)(0)

	// This increments a tuple of coordinates by 1 and handles overflows.
	def IncrementCoord (x : Vector[Int]) : Vector[Int] = {
		var y = x.toArray
		var c = 1
		for (n <- 0 until Dim) {
			y(n) += c //+ 1
			if (y(n) == K(n)) {

				y(n) = 0
			} else {
				c = 0
			}
		}
		// println("y: " + stringOf(y))//y.deep.mkString)
		y.toVector
	}

	// This finds the neighbor router that the channel with the given index should connect to
	// CONVENTION: First output channel is +x direction. Second is -x. So on for the other dimensions.
	def FindConsumerRouter (x : Vector[Int], index : Int) : Vector[Int] = {
		var y = x.toArray
		val indexC = index - C // Subtract concentration
		var arrayindex = indexC / 2;
		if (indexC % 2 == 0 && y(arrayindex) < K(arrayindex) - 1) { // Positive direction and not at the edge
			y(arrayindex) = y(arrayindex) + 1;
		}
		else if (indexC % 2 == 1 && y(arrayindex) > 0) { // Negative direction and not at the edge
			y(arrayindex) = y(arrayindex) - 1;
		}
		y.toVector
	}

	// The router should use this after taking scala and then constructing hardware.
	// def LiftCoord (x:Vector[Int]): Vec[UInt] = {
	// 	Vec[UInt](x.map(x => UInt(x, width = AddressWidth)))
	// }

	// Create all the routers with coordinates.
	// Not all routers are the same radix (this is a mesh), but the parameters we pass are the same. Synthesis should remove unconnected ports.
	// TODO fix the above.
	// println("numRouters: " + numRouters)
	var connectionsMap = new HashMap[Vector[Int], Array[Int]]()
	var busProbesMap = new HashMap[Vector[Int], BusProbe]()
    var creditBufsMap = new HashMap[Vector[Int], CreditBuffer]()
	for (n <- 0 until numRouters) {
		// println("coord: " + coord)
		var newRouter = Chisel.Module ( routerCtor(
		// var newRouter = Chisel.Module ( new SimpleRouter(
			parms.child(("Router",coord), Map(
				("routerID"->Soft(coord)),
				("numInChannels"->Soft(routerRadix)),
				("numOutChannels"->Soft(routerRadix)),
				("routerInCredits"->Soft(topoInCredits)),
				("routerOutCredits"->Soft(topoOutCredits)),
				("rfCtor"->Soft(routingFuncCtor))
			))))
		//var newBusProbe = Chisel.Module( new BusProbe(parms) ) 
		var newBusProbe = Chisel.Module( new BusProbe(parms.child("BusProbeParms", Map( ("routerRadix"->Soft(routerRadix)) )) ) ) 
        var newcreditBuf = Chisel.Module( new CreditBuffer(parms))
		routermap += coord -> newRouter
		connectionsMap += coord -> Array.fill(routerRadix)(0)
		busProbesMap += coord -> newBusProbe
        creditBufsMap += coord -> newcreditBuf
		// Create the channels. Channels are indexed by the coordinates of the router that is feeding them flits (they are output channels for the router), and an integer with the port number.
		coord = IncrementCoord(coord)
	}
	coord = Vector.fill(Dim)(0)


	// Now we go instantiate and connect channels.
	for (n <- 0 until numRouters) {
		for (i <- C until routerRadix) {

			// This is the messy part. We need to connect the same channel to the router that has it as input. Depending on i, we have to fetch the router.
			var consumerrouter = FindConsumerRouter(coord, i) 	// - C is contained here. The index refers to channels after the first C ones (the first indexed channels are I/E
			if (consumerrouter != coord) { 						// If they are equal it means that there is no appropriate neighbor router.
				//routermap(consumerrouter).io.inChannels(i) 	<> routermap(coord).io.outChannels(i) 
				routermap(consumerrouter).io.inChannels(i) 	<> creditBufsMap(coord).io.out(i)
                creditBufsMap(coord).io.in(i)                  <> routermap(coord).io.outChannels(i) 
				busProbesMap(coord).io.inFlit(i) 			:= routermap(coord).io.outChannels(i).flit  
				busProbesMap(coord).io.inValid(i) 			:= routermap(coord).io.outChannels(i).flitValid
				busProbesMap(coord).io.routerCord 			:= UInt(consumerrouter.product)
				io.cyclesChannelBusy((n*routerRadix) + i)	:= busProbesMap(coord).io.cyclesChannelBusy(i)

				// channelmap += (channelcoord, newchannel) // For delay handling
				println("Connecting router ", coord, "port ", i, " to consumer router ", consumerrouter, " port ", i, " router radix= ", routerRadix)
				println("Settng ConnectionsMap for ", consumerrouter, " port ", i," to 1")
				connectionsMap(consumerrouter)(i) = 1
			} else{
				println("Leaving consumerrouter ", consumerrouter, " port ", i, " unconnected") 
			}	
		
				
		}
		// We handle injection and ejection channels separately
		for (p <- 0 until C) {
			println("Connecting router ", coord, "port ", p, " to injection queue ", p+ (n*C))
			routermap(coord).io.inChannels(p) 	<> io.inChannels(p + (n*C))
			println("Settng ConnectionsMap for ", coord, " port ", p," to 1")
			connectionsMap(coord)(p) = 1
			println("Connecting router ", coord, "port ", p, " to ejection queue ", p+(n*C))
			io.outChannels(p + (n*C)) 				<> routermap(coord).io.outChannels(p)
		}
		io.cyclesRouterBusy(n)	:= busProbesMap(coord).io.cyclesRouterBusy
		coord 					= IncrementCoord(coord)
	}

	coord = Vector.fill(Dim)(0)
	for (n <- 0 until numRouters) {
		for (i <- C until routerRadix) {
			var consumerrouter = coord 
			println("ConnectionsMap for: ", consumerrouter, " port ", i, " = ", connectionsMap(consumerrouter)(i))
			if(connectionsMap(consumerrouter)(i) != 1){
				val NullEndpoint = Chisel.Module(new OpenSoC_ConstantEndpoint(parms.child("NullEndpoint", Map(
												( "numInChannels"->Soft(routerRadix)),
												( "numOutChannels"->Soft(routerRadix)) )) ) )
				println("Null endpoint created")
				routermap(consumerrouter).io.inChannels(i) <> NullEndpoint.io.outChannels(i)
				NullEndpoint.io.inChannels(i) <> routermap(consumerrouter).io.outChannels(i)
				connectionsMap(consumerrouter)(i) = 1
			}
		}
		coord = IncrementCoord(coord)
	}
			

	// This class instantiates the proper number of routers and channels
	// It then connects them (by calling member functions?) according to the specific topology

 	// This class also provides member routing functions with the proper interface
	// It passes a child routing function instance to each instantiated router

}

class VCCMesh(parms: Parameters) extends VCTopology(parms) {
	
	// val numRouters : Int = K.product //Math.pow(K, Dim).toInt
	val routerRadix = 2 * Dim + C
	val numChannels = numRouters * 2 * Dim  // Channels from Router to Router
	// val numIOChannels = numRouters * C // Channels from Proc to Router


	// Make sure the parameters have been given correctly.
	//assert(numRouters * C == InChannels && numRouters * C == OutChannels)

	var routermap = new HashMap[Vector[Int], VCRouter]()
	//var channelmap = new HashMap[Any,Any]()
	var coord = Vector.fill(Dim)(0)

	// This increments a tuple of coordinates by 1 and handles overflows.
	def IncrementCoord (x : Vector[Int]) : Vector[Int] = {
		var y = x.toArray
		var c = 1
		for (n <- 0 until Dim) {
			y(n) += c //+ 1
			if (y(n) == K(n)) {

				y(n) = 0
			} else {
				c = 0
			}
		}
		// println("y: " + stringOf(y))//y.deep.mkString)
		y.toVector
	}

	// This finds the neighbor router that the channel with the given index should connect to
	// CONVENTION: First output channel is +x direction. Second is -x. So on for the other dimensions.
	def FindConsumerRouter (x : Vector[Int], index : Int) : Vector[Int] = {
		var y = x.toArray
		val indexC = index - C // Subtract concentration
		var arrayindex = indexC / 2;
		if (indexC % 2 == 0 && y(arrayindex) < K(arrayindex) - 1) { // Positive direction and not at the edge
			y(arrayindex) = y(arrayindex) + 1;
		}
		else if (indexC % 2 == 1 && y(arrayindex) > 0) { // Negative direction and not at the edge
			y(arrayindex) = y(arrayindex) - 1;
		}
		y.toVector
	}

	// The router should use this after taking scala and then constructing hardware.
	// def LiftCoord (x:Vector[Int]): Vec[UInt] = {
	// 	Vec[UInt](x.map(x => UInt(x, width = AddressWidth)))
	// }

	// Create all the routers with coordinates.
	// Not all routers are the same radix (this is a mesh), but the parameters we pass are the same. Synthesis should remove unconnected ports.
	// TODO fix the above.
	// println("numRouters: " + numRouters)
	var connectionsMap = new HashMap[Vector[Int], Array[Int]]()
	var busProbesMap = new HashMap[Vector[Int], BusProbe]()
    var creditBufsMap = new HashMap[Vector[Int], CreditBuffer]()
	for (n <- 0 until numRouters) {
		// println("coord: " + coord)
		var newRouter = Chisel.Module ( routerCtor(
		// var newRouter = Chisel.Module ( new SimpleRouter(
			parms.child(("Router",coord), Map(
				("routerID"->Soft(coord)),
				("numInChannels"->Soft(routerRadix)),
				("numOutChannels"->Soft(routerRadix)),
				("routerInCredits"->Soft(topoInCredits)),
				("routerOutCredits"->Soft(topoOutCredits)),
				("rfCtor"->Soft(routingFuncCtor))
			))))
		//var newBusProbe = Chisel.Module( new BusProbe(parms) ) 
		var newBusProbe = Chisel.Module( new BusProbe(parms.child("BusProbeParms", Map( ("routerRadix"->Soft(routerRadix)) )) ) ) 
        var newcreditBuf = Chisel.Module( new CreditBuffer(parms))
		routermap += coord -> newRouter
		connectionsMap += coord -> Array.fill(routerRadix)(0)
		busProbesMap += coord -> newBusProbe
        creditBufsMap += coord -> newcreditBuf
		// Create the channels. Channels are indexed by the coordinates of the router that is feeding them flits (they are output channels for the router), and an integer with the port number.
		coord = IncrementCoord(coord)
	}
	coord = Vector.fill(Dim)(0)


	// Now we go instantiate and connect channels.
	for (n <- 0 until numRouters) {
		for (i <- C until routerRadix) {

			// This is the messy part. We need to connect the same channel to the router that has it as input. Depending on i, we have to fetch the router.
			var consumerrouter = FindConsumerRouter(coord, i) 	// - C is contained here. The index refers to channels after the first C ones (the first indexed channels are I/E
			if (consumerrouter != coord) { 						// If they are equal it means that there is no appropriate neighbor router.
				//routermap(consumerrouter).io.inChannels(i) 	<> routermap(coord).io.outChannels(i) 
				routermap(consumerrouter).io.inChannels(i) 	<> creditBufsMap(coord).io.out(i)
                creditBufsMap(coord).io.in(i)                  <> routermap(coord).io.outChannels(i) 
				busProbesMap(coord).io.inFlit(i) 			:= routermap(coord).io.outChannels(i).flit  
				busProbesMap(coord).io.inValid(i) 			:= routermap(coord).io.outChannels(i).flitValid
				busProbesMap(coord).io.routerCord 			:= UInt(consumerrouter.product)
				io.cyclesChannelBusy((n*routerRadix) + i)	:= busProbesMap(coord).io.cyclesChannelBusy(i)

				// channelmap += (channelcoord, newchannel) // For delay handling
				println("Connecting router ", coord, "port ", i, " to consumer router ", consumerrouter, " port ", i, " router radix= ", routerRadix)
				println("Settng ConnectionsMap for ", consumerrouter, " port ", i," to 1")
				connectionsMap(consumerrouter)(i) = 1
			} else{
				println("Leaving consumerrouter ", consumerrouter, " port ", i, " unconnected") 
			}	
		
				
		}
		// We handle injection and ejection channels separately
		for (p <- 0 until C) {
			println("Connecting router ", coord, "port ", p, " to injection queue ", p+ (n*C))
			routermap(coord).io.inChannels(p) 	<> io.inChannels(p + (n*C))
			println("Settng ConnectionsMap for ", coord, " port ", p," to 1")
			connectionsMap(coord)(p) = 1
			println("Connecting router ", coord, "port ", p, " to ejection queue ", p+(n*C))
			io.outChannels(p + (n*C)) 				<> routermap(coord).io.outChannels(p)
		}
		io.cyclesRouterBusy(n)	:= busProbesMap(coord).io.cyclesRouterBusy
		coord 					= IncrementCoord(coord)
	}

	coord = Vector.fill(Dim)(0)
	for (n <- 0 until numRouters) {
		for (i <- C until routerRadix) {
			var consumerrouter = coord 
			println("ConnectionsMap for: ", consumerrouter, " port ", i, " = ", connectionsMap(consumerrouter)(i))
			if(connectionsMap(consumerrouter)(i) != 1){
				val NullEndpoint = Chisel.Module(new OpenSoC_VCConstantEndpoint(parms.child("NullEndpoint", Map(
												( "numInChannels"->Soft(routerRadix)),
												( "numOutChannels"->Soft(routerRadix)) )) ) )
				println("Null endpoint created")
				routermap(consumerrouter).io.inChannels(i) <> NullEndpoint.io.outChannels(i)
				NullEndpoint.io.inChannels(i) <> routermap(consumerrouter).io.outChannels(i)
				connectionsMap(consumerrouter)(i) = 1
			}
		}
		coord = IncrementCoord(coord)
	}
			

	// This class instantiates the proper number of routers and channels
	// It then connects them (by calling member functions?) according to the specific topology

 	// This class also provides member routing functions with the proper interface
	// It passes a child routing function instance to each instantiated router

}


// This topology constructs a multi-dimensional mesh that has the same number of routers in each dimension.
// It is future work to lift this restriction, which will require a tuple or vector instead of the "RoutersPerDim" parameter.
class CFlatBfly(parms: Parameters) extends Topology(parms) {

	val routerRadix 		= K.sum - Dim + C
	val numChannels 		= numRouters * (K.sum - Dim)	// Channels from Router to Router

	var routerHashMap		= new HashMap[Vector[Int], Router]()
	var inputConnectionsMap	= new HashMap[Vector[Int], Int]()
	var busProbesMap 		= new HashMap[Vector[Int], BusProbe]()
	var coord 				= Vector.fill(Dim)(0)

	// This increments a tuple of coordinates by 1 and handles overflows.
	def incrementCoord (x : Vector[Int]) : Vector[Int] = {
		var y = x.toArray
		var c = 1
		for (n <- 0 until Dim) {
			y(n) += c //+ 1
			if (y(n) == K(n)) {
				y(n) = 0
			} else {
				c = 0
			}
		}
		// println("y: " + stringOf(y))//y.deep.mkString)
		y.toVector
	}

	// This finds the neighbor router that the channel with the given index should connect to
	// CONVENTION: The closest +x is first until the +x is exhusted, then we start from the furthest -x to the closest -x, and so on
	def findConsumerRouter (coord : Vector[Int], dim : Int, k : Vector[Int], c : Int, port : Int) : Vector[Int] = {
		var targetCoord = coord.toArray
		var coordOffset : Int = port - (c - 1)
		var dimOffset : Int = 0
		while ((dimOffset < dim) && coordOffset >= k(dimOffset)) {
			coordOffset = coordOffset - (k(dimOffset) - 1)
			dimOffset = dimOffset + 1
		}

		// val dimOffset = if (curDim == 0) { 0 } else { k.slice(0, curDim).sum - curDim }

		// val extractedTemp = port - (c - 1) - dimOffset
		if (coordOffset < (k(dimOffset) - coord(dimOffset))) {
			targetCoord(dimOffset) = coord(dimOffset) + coordOffset
		} else {
			targetCoord(dimOffset) = (k(dimOffset) - 1)  - coordOffset
		}

		targetCoord.toVector
	}

	def dotProduct[T <% Int](as: Iterable[T], bs: Iterable[T]) = {
		require(as.size == bs.size)
		(for ((a, b) <- as zip bs) yield a * b) sum
	}

	def indexify( coord : Vector[Int] , k : Vector[Int] ) : Int = {
		require((coord.size == k.size) && (coord.corresponds(k){_ < _}))
		val stride : Vector[Int] = k.indices.map( i =>
			if (i == 0) 1 else k.slice(0,i).sum
		).toVector
		dotProduct(coord, stride)
	}

	def getConnectableCoords( coord : Vector[Int] , k : Vector[Int] , dim : Int) : List[Vector[Int]] = {
		var validCoords = List[Vector[Int]]()
		for (i <- 0 until dim) {
			val x : List[Vector[Int]] = List.fill(k(i)) (coord)
			val y : List[Vector[Int]] = ((for ((e,j) <- x.zipWithIndex) yield e.updated(i,j)) filter(_ != coord))
			validCoords = validCoords ++ y
		}
		validCoords
	}

	// Create all the routers with coordinates.
	// println("numRouters: " + numRouters)
	for (n <- 0 until numRouters) {
		var newRouter = Chisel.Module ( routerCtor(
			parms.child(("Router",coord), Map(
				("routerID"->Soft(coord)),
				("numInChannels"->Soft(routerRadix)),
				("numOutChannels"->Soft(routerRadix)),
				("routerInCredits"->Soft(topoInCredits)),
				("routerOutCredits"->Soft(topoOutCredits)),
				("rfCtor"->Soft(routingFuncCtor))
			))))
		var newBusProbe = Chisel.Module( new BusProbe(parms.child("BusProbeParms", Map( ("routerRadix"->Soft(routerRadix)) )) ) ) 
		routerHashMap += coord -> newRouter
		busProbesMap += coord -> newBusProbe
		coord = incrementCoord(coord)
	}

	// Connect concentration ports
	routerHashMap.foreach{ case (key, value) =>
		val i : Int = indexify(key, K)
		var portIndex : Int = 0
		println(" i = " + i + " portIndex = " + portIndex + " numInChannels = " + routerRadix)
		for (j <- (i*C) until ((i+1)*C)) {
			//portIndex = if (i == 0) j else j % i
			value.io.inChannels(portIndex) <> io.inChannels(j)
			println("Connecting router " + key + " input " + portIndex + " to input channel " + j)
			io.outChannels(j) <> value.io.outChannels(portIndex)
			println("Connecting router " + key + " output " + portIndex + " to output channel " + j)
			io.cyclesRouterBusy(i)	:= busProbesMap(key).io.cyclesRouterBusy
			portIndex += 1
		}
		inputConnectionsMap(key) = C
	}

	// Connect Router ports
	routerHashMap.foreach{ case (key, value) => 
		for (i <- C until routerRadix) {
			val targetCoord = findConsumerRouter(key, Dim, K, C, i)
			val targetRouter = routerHashMap(targetCoord)
			val targetInputConnVal = inputConnectionsMap(targetCoord)
			value.io.outChannels(i) <> targetRouter.io.inChannels(targetInputConnVal)
			inputConnectionsMap(targetCoord) = targetInputConnVal + 1
			println("Connecting router "+ key +  " physical port "+ i + " to consumer router " + targetCoord + " port "+ targetInputConnVal + " router radix= " + routerRadix)
			
			busProbesMap(key).io.inFlit(i) 		:= value.io.outChannels(i).flit  
			busProbesMap(key).io.inValid(i) 		:= value.io.outChannels(i).flitValid
			busProbesMap(key).io.routerCord 		:= UInt(targetCoord.product)
			//io.cyclesChannelBusy((n*routerRadix) + i)	:= busProbesMap(coord).io.cyclesChannelBusy(i)
		}
	}

	println("Topology connections done!")

	// This class instantiates the proper number of routers and channels
	// It then connects them (by calling member functions?) according to the specific topology

 	// This class also provides member routing functions with the proper interface
	// It passes a child routing function instance to each instantiated router
}

// This topology constructs a multi-dimensional mesh that has the same number of routers in each dimension.
// It is future work to lift this restriction, which will require a tuple or vector instead of the "RoutersPerDim" parameter.
class VCCFlatBfly(parms: Parameters) extends VCTopology(parms) {

	val routerRadix 		= K.sum - Dim + C
	val numChannels 		= numRouters * (K.sum - Dim)	// Channels from Router to Router

	var routerHashMap		= new HashMap[Vector[Int], VCRouter]()
	var inputConnectionsMap	= new HashMap[Vector[Int], Int]()
	var busProbesMap 		= new HashMap[Vector[Int], BusProbe]()
	var coord 				= Vector.fill(Dim)(0)

	// This increments a tuple of coordinates by 1 and handles overflows.
	def incrementCoord (x : Vector[Int]) : Vector[Int] = {
		var y = x.toArray
		var c = 1
		for (n <- 0 until Dim) {
			y(n) += c //+ 1
			if (y(n) == K(n)) {
				y(n) = 0
			} else {
				c = 0
			}
		}
		// println("y: " + stringOf(y))//y.deep.mkString)
		y.toVector
	}

	// This finds the neighbor router that the channel with the given index should connect to
	// CONVENTION: The closest +x is first until the +x is exhusted, then we start from the furthest -x to the closest -x, and so on
	def findConsumerRouter (coord : Vector[Int], dim : Int, k : Vector[Int], c : Int, port : Int) : Vector[Int] = {
		var targetCoord = coord.toArray
		var coordOffset : Int = port - (c - 1)
		var dimOffset : Int = 0
		while ((dimOffset < dim) && coordOffset >= k(dimOffset)) {
			coordOffset = coordOffset - (k(dimOffset) - 1)
			dimOffset = dimOffset + 1
		}

		// val dimOffset = if (curDim == 0) { 0 } else { k.slice(0, curDim).sum - curDim }

		// val extractedTemp = port - (c - 1) - dimOffset
		if (coordOffset < (k(dimOffset) - coord(dimOffset))) {
			targetCoord(dimOffset) = coord(dimOffset) + coordOffset
		} else {
			targetCoord(dimOffset) = (k(dimOffset) - 1)  - coordOffset
		}

		targetCoord.toVector
	}

	def dotProduct[T <% Int](as: Iterable[T], bs: Iterable[T]) = {
		require(as.size == bs.size)
		(for ((a, b) <- as zip bs) yield a * b) sum
	}

	def indexify( coord : Vector[Int] , k : Vector[Int] ) : Int = {
		require((coord.size == k.size) && (coord.corresponds(k){_ < _}))
		val stride : Vector[Int] = k.indices.map( i =>
			if (i == 0) 1 else k.slice(0,i).sum
		).toVector
		dotProduct(coord, stride)
	}

	def getConnectableCoords( coord : Vector[Int] , k : Vector[Int] , dim : Int) : List[Vector[Int]] = {
		var validCoords = List[Vector[Int]]()
		for (i <- 0 until dim) {
			val x : List[Vector[Int]] = List.fill(k(i)) (coord)
			val y : List[Vector[Int]] = ((for ((e,j) <- x.zipWithIndex) yield e.updated(i,j)) filter(_ != coord))
			validCoords = validCoords ++ y
		}
		validCoords
	}

	// Create all the routers with coordinates.
	// println("numRouters: " + numRouters)
	for (n <- 0 until numRouters) {
		var newRouter = Chisel.Module ( routerCtor(
			parms.child(("Router",coord), Map(
				("routerID"->Soft(coord)),
				("numInChannels"->Soft(routerRadix)),
				("numOutChannels"->Soft(routerRadix)),
				("routerInCredits"->Soft(topoInCredits)),
				("routerOutCredits"->Soft(topoOutCredits)),
				("rfCtor"->Soft(routingFuncCtor))
			))))
		var newBusProbe = Chisel.Module( new BusProbe(parms.child("BusProbeParms", Map( ("routerRadix"->Soft(routerRadix)) )) ) ) 
		routerHashMap += coord -> newRouter
		busProbesMap += coord -> newBusProbe
		coord = incrementCoord(coord)
	}

	// Connect concentration ports
	routerHashMap.foreach{ case (key, value) =>
		val i : Int = indexify(key, K)
		var portIndex : Int = 0
		println(" i = " + i + " portIndex = " + portIndex + " numInChannels = " + routerRadix)
		for (j <- (i*C) until ((i+1)*C)) {
			//portIndex = if (i == 0) j else j % i
			value.io.inChannels(portIndex) <> io.inChannels(j)
			println("Connecting router " + key + " input " + portIndex + " to input channel " + j)
			io.outChannels(j) <> value.io.outChannels(portIndex)
			println("Connecting router " + key + " output " + portIndex + " to output channel " + j)
			io.cyclesRouterBusy(i)	:= busProbesMap(key).io.cyclesRouterBusy
			portIndex += 1
		}
		inputConnectionsMap(key) = C
	}

	// Connect Router ports
	routerHashMap.foreach{ case (key, value) => 
		for (i <- C until routerRadix) {
			val targetCoord = findConsumerRouter(key, Dim, K, C, i)
			val targetRouter = routerHashMap(targetCoord)
			val targetInputConnVal = inputConnectionsMap(targetCoord)
			value.io.outChannels(i) <> targetRouter.io.inChannels(targetInputConnVal)
			inputConnectionsMap(targetCoord) = targetInputConnVal + 1
			println("Connecting router "+ key +  " physical port "+ i + " to consumer router " + targetCoord + " port "+ targetInputConnVal + " router radix= " + routerRadix)
			
			busProbesMap(key).io.inFlit(i) 		:= value.io.outChannels(i).flit  
			busProbesMap(key).io.inValid(i) 		:= value.io.outChannels(i).flitValid
			busProbesMap(key).io.routerCord 		:= UInt(targetCoord.product)
			//io.cyclesChannelBusy((n*routerRadix) + i)	:= busProbesMap(coord).io.cyclesChannelBusy(i)
		}
	}

	println("Topology connections done!")

	// This class instantiates the proper number of routers and channels
	// It then connects them (by calling member functions?) according to the specific topology

 	// This class also provides member routing functions with the proper interface
	// It passes a child routing function instance to each instantiated router
}
