import Prelude._
import FormulaList.{One, Chc}

object Main extends App {
  type VImpl[A] = FormulaList[A]
  val V = FormulaList
  
  def f(s: String)(a: Int): VImpl[Int] = {
    Chc(tag(s), One(a + 1), One(a + 100))
  }

  val s = List(1,2,3)
  println(s :+ 4)
  println(s.foldRight(0)((a:Int, b:Int) => a + b))
  
  
  var x = V.One(0)
  println(x)
  x = x.flatMap(f("A"))
  println(x)
  x = x.flatMap(f("B"))
  println(x)

}

