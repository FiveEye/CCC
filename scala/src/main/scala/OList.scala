package fe.CCC

import FormulaList.{One, Chc}



class OList[A] {
  def size() = OList.size(this)
  def get(i: Int) = OList.get(this, i)
}

object OList {
  type VImpl[A] = FormulaList[A]
  val V = FormulaList

  case class ONil[A]() extends OList[A]
  case class OCons[A](v: VImpl[Option[A]], next: OList[A]) extends OList[A]

  def size[A](l: OList[A]): VImpl[Int] = l match {
    case ONil() => One(0)
    case OCons(v,next) => {
      val sn = size(next) 
      v flatMap {
        case Some(a) => sn map {(x: Int) => x + 1}
        case None => sn
      }
    }
  }

  def get[A](l: OList[A], i: Int): VImpl[Option[A]] = l match {
    case ONil() => One(None)
    case OCons(v,next) => {
      if (i < 0) 
        One(None)
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


