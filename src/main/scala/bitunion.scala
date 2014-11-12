//////////////////////////////////////////////////////////////////////////
// Open System-on-a-Chip (OpenSoC) is Copyright (c) 2014,               //
// The Regents of the University of California, through Lawrence        //
// Berkeley National Laboratory (subject to receipt of any required     //
// approvals from the U.S. Dept. of Energy).  All rights reserved.      //
//                                                                      //
// Portions may be copyrighted by others, as may be noted in specific   //
// copyright notices within specific files.                             //
//                                                                      //
// AUTHOR: J. Bachan, F. Fatollahi-Fard                                 //
//////////////////////////////////////////////////////////////////////////

package OpenSoC

import Chisel._

class BitUnion(tag2data: Map[String, Chisel.Data]) {
  
  private val codeWidth = log2Up(tag2data.size)
  private val tailWidth = tag2data.values.map(_.toBits.getWidth).foldLeft(0)(math.max)
  private val tag2code = {
    val s = tag2data.keys.toArray.sorted
    Map(s zip s.indices:_*)
  }
  
  val width : Int = codeWidth + tailWidth
  
  def tagEquals(tag: String, x: Chisel.Bits) : Chisel.Bool = {
    val code = x.apply(0, codeWidth-1)
    code === Chisel.UInt(tag2code(tag))
  }

  def pack[T <: Chisel.Data](tag: String, x: T) : Chisel.Bits = {
    // val fred = Vec.fill(this.width) { UInt(width = 1) }

    // // fred(0, codeWidth - 1).toBits := UInt(tad2code(tag), width = codeWidth)
    val code = UInt(tag2code(tag), width = codeWidth)
    // for (i <- 0 until codeWidth) {
    //   fred(i) := code(i)
    // }

    val xb = x.toBits
    // // println("xb getWidth: " + xb.getWidth + "\twidth: " + xb.width)
    // for (i <- 0 until xb.getWidth) {
    //   fred(codeWidth + i) := xb(i)
    // }

    // println("code width: " + codeWidth + "\txb getWidth: " + xb.getWidth + "\tthis.width: " + this.width)
    val zerosWidth = this.width - (codeWidth + xb.getWidth)
    // for (i <- (codeWidth + xb.getWidth) until this.width) {
    //   fred(i) := UInt(0)
    // }

    // fred.toBits
    if (zerosWidth == 0) {
      Cat(xb, code)
    } else {
      val zeros = UInt(0, width = zerosWidth)
      Cat(zeros, xb, code)
    }
  }

  def unpack[T <: Chisel.Data](tag: String, x: Chisel.Bits) : T = {
    val data = tag2data(tag).clone
    // println("data getWidth: " + data.toBits.getWidth + "\twidth: " + data.width)
    data.fromBits(x.apply(codeWidth+data.toBits.getWidth-1, codeWidth)).asInstanceOf[T]
  }
  
  def whenTag[T <: Chisel.Data](tag: String, x: Chisel.Bits) (block: T => Unit) = {
    Chisel.when(tagEquals(tag, x)) { block(unpack[T](tag, x)) }
  }
}
