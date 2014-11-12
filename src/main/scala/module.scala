package OpenSoC

import Chisel._
import scala.collection.mutable.ListBuffer

abstract class Module(val parameters: Parameters) extends Chisel.Module {
//	def osocKids(): HashMap[Any,Module]
}

object NyanCat {
	def apply(original : UInt, mapVals : Map[Int, UInt]) : Chisel.UInt = {
		val listSortedVals : List[(Int, UInt)] = mapVals.toList.sortBy(_._1)
		var catedList : ListBuffer[UInt] = new ListBuffer[UInt]()

		if ((listSortedVals(0)._1 != 0) || (listSortedVals(0)._1 != (listSortedVals(0)._2.getWidth - 1))) {
			catedList.prepend(original(listSortedVals(0)._1 - ((listSortedVals(0)._2.getWidth - 1)), 0))
		}
		println(catedList)
		for (i <- 0 until listSortedVals.length) {
			// catedList = ListBuffer[UInt](listSortedVals(i)._2) :+ catedList
			catedList.prepend(listSortedVals(i)._2)
			if (i < (listSortedVals.length - 1)) {
				// catedList = ListBuffer[UInt](original(listSortedVals(i+1)._1 - (listSortedVals(i+1)._2.getWidth - 1), listSortedVals(i)._1)) :+ catedList
				catedList.prepend(original(listSortedVals(i+1)._1 - (listSortedVals(i+1)._2.getWidth - 1), listSortedVals(i)._1))
			} else {
				if (listSortedVals(i)._1 != (original.getWidth - 1))
				// catedList = ListBuffer[UInt](original(original.getWidth - 1, listSortedVals(i)._1)) :+ catedList
				catedList.prepend(original(original.getWidth - 1, listSortedVals(i)._1))
			}
		}

		Cat(catedList.toSeq)
	}
}
