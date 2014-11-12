//////////////////////////////////////////////////////////////////////////
// Open System-on-a-Chip (OpenSoC) is Copyright (c) 2014,               //
// The Regents of the University of California, through Lawrence        //
// Berkeley National Laboratory (subject to receipt of any required     //
// approvals from the U.S. Dept. of Energy).  All rights reserved.      //
//                                                                      //
// Portions may be copyrighted by others, as may be noted in specific   //
// copyright notices within specific files.                             //
//                                                                      //
// AUTHOR: J. Bachan                                                    //
//////////////////////////////////////////////////////////////////////////

package OpenSoC

abstract class PValue {
  val value: Any
}

case class Soft(override val value:Any) extends PValue
case class Hard(override val value:Any) extends PValue

class ParameterUndefinedException(f:Any) extends RuntimeException("Parameter " + f + " undefined.")

/* This and PUpGet are just to give nice syntax of doing a function
 * call and cast to some type T all in one shot:
 *   def something(up:PUpGet) = up[Int]("a") + up[Int]("b")
 * as opposed to:
 *   def something(up:Any=>Any) = up("a").asInstanceOf[Int] + up("b").asInstanceOf[Int]
 */
abstract class PGet {
  def apply[T](f:Any):T
}

abstract class PUpGet {
  def apply[T](f:Any, path:List[Any]=Nil):T
}

/* The Parameters constructor should not be called by user code*/
class Parameters(
    /* The globally-unique name for this module is its path down the
     * tree of names. Leaf nodes are at the head of the list. The top
     * level is just the Nil list.
     */
    val path: List[Any],
    /* The function used by this parameters instance to lookup values.
     * Its arguments are the parameter name and the name of the child
     * asking. The child path is relative to _this_ instance, not a global
     * path therefor Nil points to _this_.
     */
    private val _fun:(Any, List[Any]) => PValue
  ) extends PGet {
  
  def get[T](field:Any):T = _fun(field, Nil).value.asInstanceOf[T]
  
  def apply[T](field:Any):T = _fun(field, Nil).value.asInstanceOf[T]
  
  /* sub will build a new Parameters object from this one by:
   *   - (optionally) pushing a new name onto the path.
   *   - applying a masking function to our internally stored _fun.
   */
  def sub(childName:Option[Any], funmask:(Any,List[Any],PUpGet)=>PValue) = {
    val childNames = childName match {
      case Some(nm) => List(nm)
      case None => Nil
    }
    
    // The name path for the new parms object
    val subpath = childNames ::: path
    
    // The _fun value stored in the new parms object, we are its parent
    // so the `up` we give it must call into us.
    def subfn(f:Any, p:List[Any]) = {
      // This will be passed to funmask as `up`
      object up extends PUpGet {
        def apply[T](f1:Any, p1:List[Any]):T = {
          // call our stored function to lookup the value with our name
          // added to the name stack
          _fun(f1, p1 ::: childNames).value.asInstanceOf[T]
        }
      }
      
      // Query the funmask which might just call `up`
      val y = funmask(f, p, up)
      // Also query `up` directly so we can compare the values
      val y1 = {
        try { _fun(f, p ::: childNames) }
        catch { case ex:ParameterUndefinedException => y }
      }

      // println("Child: " + subpath + "\tParameter " + f + ": " + y1)
      
      // Return y. Also assert the values are equal when hardness is involved
      y1 match {
        case Soft(_) => y
        case Hard(_) => {
          assert(y.value == y1.value, "Hard parameter " + f + " has been overriden for " + subpath.toString + ".")
          y1 // return y1 and NOT y to maintain hardness
        }
      }
    }
    
    new Parameters(subpath, subfn)
  }
  
  // Lift a Map to be a funmask.
  def makeFunMask[T](mask:Map[T,PValue]) = {
    val m = mask.asInstanceOf[Map[Any,PValue]]
    (f:Any, p:List[Any], up:PUpGet) => {
      m.get(f) match {
        case Some(y) => y
        case None => Soft(up[Any](f,p))
      }
    }
  }
  
  // Lift a PartialFunction to be a funmask.
  def makeFunMask[T](mask:PartialFunction[T,PValue]) = {
    val m = mask.asInstanceOf[PartialFunction[Any,PValue]]
    (f:Any, p:List[Any], up:PUpGet) => {
      if(m.isDefinedAt(f))
        m.apply(f)
      else
        Soft(up[Any](f,p))
    }
  }
  
  def child(name:Any, funmask:(Any,List[Any],PUpGet)=>PValue) =
    sub(Some(name), funmask)
  
  def child[T](name:Any, mapmask:Map[T,PValue] = Map()) =
    sub(Some(name), makeFunMask(mapmask))
  
  def childPartial(name:Any, mask:PartialFunction[Any,PValue]) =
    sub(Some(name), makeFunMask(mask))
  
  def mask(funmask:(Any,List[Any],PUpGet)=>PValue) =
    sub(None, funmask)
  
  def mask[T](mapmask:Map[T,PValue]) =
    sub(None, makeFunMask(mapmask))
  
  def maskPartial(mask:PartialFunction[Any,PValue]) =
    sub(None, makeFunMask(mask))
}

object Parameters {
  val empty = new Parameters(Nil, (f,p)=>throw new ParameterUndefinedException(f))
}

/* try it out!
val p = Parameters.empty
val p1 = p.childPartial("Parent", {
  case "a" => Hard(1)
  case "b" => Hard(2)
  case "c" => Soft(5)
})
val p2 = p1.child("Child", (f,kid,up) => {
  Soft(f match {
    case "c" => up[Int]("a") + up[Int]("b")
    case _ => up[Any](f,kid)
  })
})

println(p2[Int]("a"))
println(p2[Int]("b"))
println(p2[Int]("c"))
*/
