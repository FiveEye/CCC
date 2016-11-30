package fe.CCC

class OList[A] {
  type C = A
  val vf = OList
  val va = this

  def size() = OList.size(this)
  def get(i: Int) = OList.get(this, i)
}

object OList {
  
  val V = FormulaList
  
  /*
  def One[A <: List[_]](one: A): OList[A] = ???
  
  def Chc[A](s: Formula, y: VImpl[A], n: VImpl[A]): VImpl[A] = ???
  
  def flatMap[A,B](va: VImpl[A], f: (A)=>VImpl[B]): VImpl[B] = ???
  def map[A,B](va: VImpl[A], f: (A)=>B): VImpl[B] = flatMap(va, (a: A) => One(f(a)))
  
  def isOne[A](t: VImpl[A]): Option[A] = ???
  def evaluate[A](t: VImpl[A], s: Set[String]): A = ???
  
  def vToString[A](t: VImpl[A]): String = ???
  */
  case class ONil[A]() extends OList[A]
  case class OCons[A](v: V.VImpl[Option[A]], next: OList[A]) extends OList[A]

  def size[A](l: OList[A]): V.VImpl[Int] = l match {
    case ONil() => V.One(0)
    case OCons(v,next) => {
      val sn = size(next) 
      v flatMap {
        case Some(a) => sn map {(x: Int) => x + 1}
        case None => sn
      }
    }
  }

  def get[A](l: OList[A], i: Int): V.VImpl[Option[A]] = l match {
    case ONil() => V.One(None)
    case OCons(v,next) => {
      if (i < 0) 
        V.One(None)
      else {
        val sn = get(next, i)
        val sm = get(next, i - 1)
        v flatMap {
          case Some(a) => if (i == 0) V.One(Some(a)) else sm
          case None => sn
        }
      }
    }
  }
}


