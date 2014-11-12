package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

// abstract class ChannelQ(parms: Parameters) extends Module(parms) {
// 	val queueDepth = parms.get[Int]("queueDepth")
// 	val flitWidth : Int = io.in.flit.getWidth
	
// 	val io = new Bundle {
// 		val in = new Channel(parms)
// 		val out = new Channel(parms).flip()
// 	}
// }

class InjectionChannelQ(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val in = new Channel(parms)
		val out = new ChannelVC(parms).flip()
	}

	val queueDepth = parms.get[Int]("queueDepth")
	val numVCs = parms.get[Int]("numVCs")
	val vcArbCtor = parms.get[Parameters=>Arbiter]("vcArbCtor")

	val flitWidth : Int = io.in.flit.getWidth
	
	val creditGen = Chisel.Module( new CreditGen( parms.child("MyGen")) ) 
	val creditCons = (0 until numVCs).map( x =>
		Chisel.Module( new CreditCon( parms.child("MyCon", Map(
			("numCreds"->Soft(queueDepth))))) ) )
	val vcArbiter = Chisel.Module( vcArbCtor(parms.child("VCArbIQ", Map(
			("numReqs"->Soft(numVCs))
		))) )

	val queue = Chisel.Module( new Chisel.Queue(new Flit(parms), queueDepth) )
	val reassemblyReg = Reg(UInt(0, width=flitWidth))
	val chosenReg = Reg(UInt(0, Chisel.log2Up(numVCs)))
	chosenReg := vcArbiter.io.chosen
	val chosen = vcArbiter.io.chosen
	val delayGrant = (0 until numVCs).map( e =>
		Reg(Bool(false))
	)
	delayGrant.zipWithIndex.foreach{ case (e,i) =>
		e := vcArbiter.io.requests(i).grant
	}
	
	val flitifiedUInt = Flit.fromBits(reassemblyReg, parms)
	
	creditGen.io.outCredit <> io.in.credit
	queue.io.enq.valid := creditGen.io.outReady
	creditGen.io.inGrant := queue.io.deq.ready && queue.io.deq.valid
	queue.io.enq.bits <> io.in.flit
	
	creditCons.zipWithIndex.foreach{ case (e,i) => e.io.inCredit <> io.out.credit(i) }
	creditCons.zipWithIndex.foreach{ case (e,i) => e.io.inValid := delayGrant(i)}
	vcArbiter.io.requests.zipWithIndex.foreach{ case (e,i) => e.request := creditCons(i).io.outCredit }

	when( flitifiedUInt.isTail() ) {
		vcArbiter.io.requests(chosenReg).releaseLock := Bool(true)
		// vcArbiter.io.requests(chosen).releaseLock := Bool(true)
	} .otherwise {
		vcArbiter.io.requests.foreach{ case(e) => e.releaseLock := Bool(false) }
	}

	queue.io.deq.ready := vcArbiter.io.resource.valid
	vcArbiter.io.resource.ready := queue.io.deq.valid

	val replaceVC = Chisel.Module( new ReplaceVCPort( parms ) )
	replaceVC.io.oldFlit <> queue.io.deq.bits
	replaceVC.io.newVCPort := chosen //chosenReg
	reassemblyReg := replaceVC.io.newFlit.toBits

	io.out.flit <> flitifiedUInt
}

class EjectionChannelQ(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val in = new ChannelVC(parms)
		val out = new Channel(parms).flip()
	}
	val queueDepth = parms.get[Int]("queueDepth")
	val numVCs = parms.get[Int]("numVCs")
	val flitWidth : Int = io.in.flit.getWidth
	val creditGens = (0 until numVCs).map( x =>
		Chisel.Module( new CreditGen( parms.child("MyGen")) ) )
	val creditCon = Chisel.Module( new CreditCon( parms.child("MyCon", Map(
			("numCreds"->Soft(queueDepth))))) )

	val queue = Chisel.Module( new Chisel.Queue(new Flit(parms), queueDepth*numVCs) ) // Hack to account for VC credits
	
	creditGens.zipWithIndex.foreach{ case (e,i) => e.io.outCredit <> io.in.credit(i) }
	queue.io.enq.valid := creditGens.map(_.io.outReady).reduceLeft( _ || _ )
	creditGens.map(_.io.inGrant := queue.io.deq.ready && queue.io.deq.valid)
	queue.io.enq.bits <> io.in.flit
	
	creditCon.io.inCredit <> io.out.credit
	creditCon.io.inValid := queue.io.deq.valid
	queue.io.deq.ready := creditCon.io.outCredit
	io.out.flit <> queue.io.deq.bits

}

class VCIEChannelQ(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val in = new Channel(parms)
		val out = new Channel(parms).flip()
	}
	val queueDepth = parms.get[Int]("queueDepth")
	val flitWidth : Int = io.in.flit.getWidth

	val iQueue = Chisel.Module ( new InjectionChannelQ( parms.child("iQ", Map(
		("queueDepth"->Soft(queueDepth))
	))) )
	val eQueue = Chisel.Module ( new EjectionChannelQ( parms.child("eQ", Map(
		("queueDepth"->Soft(queueDepth))
	))) )

	iQueue.io.in <> io.in
	eQueue.io.in <> iQueue.io.out
	io.out <> eQueue.io.out
}


class GenericChannelQ(parms: Parameters) extends Module(parms) {
	val io = new Bundle {
		val in = new Channel(parms)
		val out = new Channel(parms).flip()
	}
	val queueDepth = parms.get[Int]("queueDepth")
	val flitWidth : Int = io.in.flit.getWidth

	val creditGen = Chisel.Module ( new CreditGen( parms.child("MyGen")) )
	val creditCon = Chisel.Module ( new CreditCon( parms.child("MyCon", Map(
		("numCreds"->Soft(queueDepth))))) )

	val queue = Chisel.Module( new Chisel.Queue(new Flit(parms), queueDepth) )
	// val isTailQueue = Chisel.Module( new Chisel.Queue(new Bool(), queueDepth) )
	
	creditGen.io.outCredit <> io.in.credit
	queue.io.enq.valid := creditGen.io.outReady
	creditGen.io.inGrant := queue.io.deq.ready && queue.io.deq.valid//queue.io.enq.ready
	// isTailQueue.io.enq.valid := creditGen.io.inGrant && isTailQueue.io.enq.ready
	// isTailQueue.io.enq.bits <> creditGen.io.inIsTail
	queue.io.enq.bits <> io.in.flit
	
	creditCon.io.inCredit <> io.out.credit
	creditCon.io.inValid := queue.io.deq.ready && queue.io.deq.valid//queue.io.deq.valid //&& isTailQueue.io.deq.valid
	queue.io.deq.ready := creditCon.io.outCredit
	// isTailQueue.io.deq.ready := creditCon.io.outCredit && creditCon.io.inValid
	// creditCon.io.outIsTail := isTailQueue.io.deq.bits
	io.out.flit <> queue.io.deq.bits
	
	// queue.count
	// isTailQueue.count

	// val destCordWidth = parms.get[Int]("destCordWidth")
	// val destCordDim = parms.get[Int]("destCordDim")
	// val destination = Vec.fill(destCordDim){UInt(width = destCordWidth)}
}

class ChannelQTest(c: GenericChannelQ) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	val queueDepth : Int = c.queueDepth
	val flitWidth : Int = c.flitWidth
	
	val randoms = (0 to 4*queueDepth).map(x => Random.nextInt(Math.pow(2, flitWidth).toInt))
	
	// printf("Flit Width: %d\n", flitWidth)

	// printf("destCordWidth: %d\t destCordDim: %d\n", c.destCordWidth, c.destCordDim)
	// printf("Destination(1) Width: %d\t getWidth: %d\n", c.destination(1).width, c.destination(1).getWidth)
	// printf("Destination Width: %d\t getWidth: %d\n", c.destination.toBits.width, c.destination.toBits.getWidth)

	for (i <- 0 to 4*queueDepth) printf("%d:\t0x%X (%d)\n", i, randoms(i), randoms(i))
	printf("---\n")
	printf("queueDepth: %d\n", queueDepth)
	printf("flitWidth: %d\n", flitWidth)

	// peek(c.creditCon.credCount)
	poke(c.io.in.credit.ready, 0)
	expect(c.io.in.credit.valid, 1)
	poke(c.io.out.credit.valid, 0)
	expect(c.io.out.credit.ready, 0)
	step(1)
	printf("---\n")
	
	printf("--- First Round: Filling the FIFO ---\n")
	for (i <- 0 to 2*queueDepth) {
		printf("Value of i: %d\n", i)
		// printf("Randoms x%h\n", randoms(1))
		// peek(c.creditCon.credCount)
		poke(c.io.in.flit, Array(BigInt(randoms(i))))
		poke(c.io.in.credit.ready, 1)
		poke(c.io.out.credit.valid, 0)
		step(1)
		// peek(c.creditCon.credCount)
		expect(c.io.in.credit.valid, (i < 2*queueDepth-1))
		expect(c.io.out.credit.ready, 1)
		if (i > queueDepth)	expect(c.io.out.flit, Array(BigInt(randoms(queueDepth))))
		else 				expect(c.io.out.flit, Array(BigInt(randoms(i))))
		printf("---\n")

	}

	printf("---\n")

	printf("--- Second Round: Emptying the FIFO ---\n")
	for (i <- 2*queueDepth to (4*queueDepth)) {
		// peek(c.creditCon.credCount)
		poke(c.io.in.credit.ready, (i == 2*queueDepth+1))
		poke(c.io.in.flit, Array(BigInt(randoms(2*queueDepth+1))))
		poke(c.io.out.credit.valid, 1)
		step(1)
		printf("Value of i: %d\n", i)
		expect(c.io.in.credit.valid, (i != 2*queueDepth))
		expect(c.io.out.credit.ready, (i < 3*queueDepth))
		if (i < 3*queueDepth) 		expect(c.io.out.flit, Array(BigInt(randoms(i - queueDepth))))
		else 						expect(c.io.out.flit, Array(BigInt(randoms(queueDepth))))
		printf("---\n")
	}
	step(1)
	expect(c.io.out.credit.ready, 0)
	// printf("Size of Output Flit: %d\n",c.io.out.flit.width)
	// printf("Size of Input Flit: %d\n",c.io.in.flit.width)
}

class VCChannelQTest(c: VCIEChannelQ) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	val queueDepth : Int = c.queueDepth
	val flitWidth : Int = c.flitWidth
	
	val randoms = (0 to 4*queueDepth).map(x => Random.nextInt(Math.pow(2, flitWidth).toInt))
	
	// printf("Flit Width: %d\n", flitWidth)

	// printf("destCordWidth: %d\t destCordDim: %d\n", c.destCordWidth, c.destCordDim)
	// printf("Destination(1) Width: %d\t getWidth: %d\n", c.destination(1).width, c.destination(1).getWidth)
	// printf("Destination Width: %d\t getWidth: %d\n", c.destination.toBits.width, c.destination.toBits.getWidth)

	for (i <- 0 to 4*queueDepth) printf("%d:\t0x%X (%d)\n", i, randoms(i), randoms(i))
	printf("---\n")
	printf("queueDepth: %d\n", queueDepth)
	printf("flitWidth: %d\n", flitWidth)

	// peek(c.creditCon.credCount)
	poke(c.io.in.credit.ready, 0)
	expect(c.io.in.credit.valid, 1)
	poke(c.io.out.credit.valid, 0)
	expect(c.io.out.credit.ready, 0)
	step(1)
	printf("---\n")
	
	printf("--- First Round: Filling the FIFO ---\n")
	for (i <- 0 to 2*queueDepth) {
		printf("Value of i: %d\n", i)
		// printf("Randoms x%h\n", randoms(1))
		// peek(c.creditCon.credCount)
		poke(c.io.in.flit, Array(BigInt(randoms(i))))
		poke(c.io.in.credit.ready, 1)
		poke(c.io.out.credit.valid, 0)
		step(1)
		// peek(c.creditCon.credCount)
		expect(c.io.in.credit.valid, (i < 2*queueDepth-1))
		expect(c.io.out.credit.ready, 1)
		if (i > queueDepth)	expect(c.io.out.flit, Array(BigInt(randoms(queueDepth))))
		else 				expect(c.io.out.flit, Array(BigInt(randoms(i))))

		peek(c.io.in.flit) //c.iQueue.queue.io.enq.bits)

		printf("---\n")

	}

	printf("---\n")

	printf("--- Second Round: Emptying the FIFO ---\n")
	for (i <- 2*queueDepth to (4*queueDepth)) {
		// peek(c.creditCon.credCount)
		poke(c.io.in.credit.ready, (i == 2*queueDepth+1))
		poke(c.io.in.flit, Array(BigInt(randoms(2*queueDepth+1))))
		poke(c.io.out.credit.valid, 1)
		step(1)
		printf("Value of i: %d\n", i)
		expect(c.io.in.credit.valid, (i != 2*queueDepth))
		expect(c.io.out.credit.ready, (i < 3*queueDepth))
		if (i < 3*queueDepth) 		expect(c.io.out.flit, Array(BigInt(randoms(i - queueDepth))))
		else 						expect(c.io.out.flit, Array(BigInt(randoms(queueDepth))))
		printf("---\n")
	}
	step(1)
	expect(c.io.out.credit.ready, 0)
	// printf("Size of Output Flit: %d\n",c.io.out.flit.width)
	// printf("Size of Input Flit: %d\n",c.io.in.flit.width)
}

class VCIQTest(c: InjectionChannelQ) extends Tester(c) {
	implicit def bool2BigInt(b:Boolean) : BigInt = if (b) 1 else 0

	val queueDepth : Int = c.queueDepth
	val flitWidth : Int = c.flitWidth
	
	val randoms = (0 to 4*queueDepth).map(x => Random.nextInt(Math.pow(2, flitWidth).toInt))
	
	// printf("Flit Width: %d\n", flitWidth)

	// printf("destCordWidth: %d\t destCordDim: %d\n", c.destCordWidth, c.destCordDim)
	// printf("Destination(1) Width: %d\t getWidth: %d\n", c.destination(1).width, c.destination(1).getWidth)
	// printf("Destination Width: %d\t getWidth: %d\n", c.destination.toBits.width, c.destination.toBits.getWidth)

	for (i <- 0 to 4*queueDepth) printf("%d:\t0x%X (%d)\n", i, randoms(i), randoms(i))
	printf("---\n")
	printf("queueDepth: %d\n", queueDepth)
	printf("flitWidth: %d\n", flitWidth)

	// peek(c.creditCon.credCount)
	poke(c.io.in.credit.ready, 0)
	expect(c.io.in.credit.valid, 1)
	// poke(c.io.out.credit.valid, 0)
	// expect(c.io.out.credit.ready, 0)
	step(1)
	printf("---\n")
	
	printf("--- First Round: Filling the FIFO ---\n")
	for (i <- 0 to 2*queueDepth) {
		printf("Value of i: %d\n", i)
		// printf("Randoms x%h\n", randoms(1))
		// peek(c.creditCon.credCount)
		poke(c.io.in.flit, Array(BigInt(randoms(i))))
		poke(c.io.in.credit.ready, 1)
		// poke(c.io.out.credit.valid, 0)
		step(1)
		// peek(c.creditCon.credCount)
		expect(c.io.in.credit.valid, (i < 2*queueDepth-1))
		// expect(c.io.out.credit.ready, 1)
		if (i > queueDepth)	expect(c.io.out.flit, Array(BigInt(randoms(queueDepth))))
		else 				expect(c.io.out.flit, Array(BigInt(randoms(i))))

		peek(c.io.in.flit) //c.iQueue.queue.io.enq.bits)

		printf("---\n")

	}
	poke(c.io.in.credit.ready, 0)
	step(10)
}
