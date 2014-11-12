package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

/*
abstract class SwitchParams {
	val NumInPorts = UInt()
	val NumOutPorts = UInt()
	val FlitWidth = UInt()
	val Implementation = UInt() // Should be Enum
}
*/

class Switch[T <: Data](val gen: T, parms: Parameters) extends Module(parms) {
	val numInPorts 	= parms.get[Int]("numInPorts")
	val numOutPorts	= parms.get[Int]("numOutPorts")
	val io = new Bundle {
		val inPorts = Vec.fill(numInPorts) {gen.clone.asInput}
		val outPorts = Vec.fill(numOutPorts) {gen.clone.asOutput}
		val sel = Vec.fill(numOutPorts) {UInt(width = log2Up(numInPorts))}.asInput
	}
	for( i <- 0 until numOutPorts) {
		var m = Chisel.Module (
			new MuxN[T](gen,
				parms.child(("SwitchMux", i), Map(("n"->Soft(numInPorts))))
			)
		)
		m.io.ins <> io.inPorts
		m.io.sel := io.sel(i)
		io.outPorts(i) := m.io.out
	}
}

class SwitchTest(c: Switch[UInt]) extends MapTester(c, Array(c.io)) {
	defTests {
		var allGood = true
		val numInPorts : Int = c.numInPorts
		val numOutPorts : Int = c.numOutPorts
		val vars = new HashMap[Node, Node]()
		val ins = (1 to numInPorts).map(x => Random.nextInt(Math.pow(2,c.gen.getWidth).toInt))
		
		for ( i <- 0 until numInPorts) {
			vars(c.io.inPorts(i)) = UInt(ins(i))
		}

		for ( i <- 0 until numOutPorts) {
			for (j <- 0 until numInPorts) {
				vars(c.io.sel(i)) = UInt(j)
				vars(c.io.outPorts(i)) = UInt(ins(j))
				allGood &= step(vars)
			}
		}
		for ( i <- 0 until numOutPorts) {
			for (j <- numInPorts-1 until -1 by -1) {
				vars(c.io.sel(i)) = UInt(j)
				vars(c.io.outPorts(i)) = UInt(ins(j))
				allGood &= step(vars)
			}
		}
		allGood
	}
}

class MuxN[T <: Data](val gen: T, parms: Parameters) extends Module(parms) {
	val n = parms.get[Int]("n")
	val io = new Bundle {
		val ins = Vec.fill(n) {gen.clone.asInput}
		val sel = UInt(INPUT, log2Up(n))
		val out = gen.clone.asOutput
	}

	io.out := io.ins(io.sel)
}

class MuxNTest(c: MuxN[UInt]) extends MapTester(c, Array(c.io)) {
	defTests {
		var allGood = true
		val n : Int = c.n
		val vars = new HashMap[Node, Node]()
		val ins = (1 to n).map(x => Random.nextInt(Math.pow(2,c.gen.getWidth).toInt))
		
		for ( s <- 0 until n ) {
			for ( i <- 0 until n ) {
				vars(c.io.ins(i)) = UInt(ins(i))
			}
			vars(c.io.sel) = UInt(s)
			vars(c.io.out) = UInt(ins(s))
			allGood &= step(vars)
		}
		allGood
	}
}

/*class DeMux[T <: Data](parms: Parameters) extends OpenSoC.Module {
	super(parms)
	val n = parms.get[Int]("n")
	val io = new Bundle {
		val input = new T().asInput
		val sel = UInt(INPUT, log2Up(n))
		val outputs = Vec.fill(n) {new T().asOutput}
	}

	io.outputs[io.sel] := io.input
}*/
