package fe.CCC

import FormulaList.{One, Chc}
import LazyFormulaList.{LazyNil, LazyCons}
import OList.{ONil, OCons}

object Main extends App {
  type VImpl[A] = FormulaList[A]
  val V = FormulaList
  
  def f(s: String)(a: Int): VImpl[Int] = {
    Chc(tag(s), One(a + 1), One(a + 100))
  }

  println("--- Lazy ---")
  val lazyInt = Lazy(10)
  println(lazyInt.isDone, lazyInt)
  
  lazyInt()
  println(lazyInt.isDone, lazyInt)

  val y = 1 + lazyInt
  println(y)
  
  println("--- FormulaList ---")
  var x = One(0)
  println(x)
  x = x.flatMap(f("A"))
  println(x)
  x = x.flatMap(f("B"))
  println(x)
  println(x(Set()))
  println(x(Set("A")))
  println(x(Set("B")))
  println(x(Set("A", "B")))
  //println((tag("a") & !tag("b")).evaluate(Set("a")))
  //println(Set(1,2,3) &~ Set(2,3,4))
  

  println("--- LazyFormulaList ---")
  def genNat(n: Int): LazyFormulaList[Int] = {
    LazyCons(tag(n.toString), n, genNat(n+1))
  }
  
  def genFib(n: Int, a: Int, b: Int): LazyFormulaList[Int] = {
    LazyCons(tag("n=" + n.toString), a, genFib(n+1, b, a+b))
  }
  
  val lazyFL = genFib(0,0,1).map(x => -x)
  //println(lazyFL(Set("n=0")))
  //println(lazyFL(Set("n=1")))
  //println(lazyFL(Set("n=2")))
  println(lazyFL(Set("n=3")))
  println(lazyFL(Set("n=4")))
  println(lazyFL(Set("n=5")))
  println(lazyFL(Set("n=6")))
  //println(lazyFL(Set("n=7")))
  //println(lazyFL(Set("n=8")))
  println(lazyFL)
  
  
  val a = LazyNil(1)
  val b = LazyNil(2)
  val c = LazyFormulaList.Chc(tag("a"), a, b)
  println(c(Set("a")))
  println(c(Set()))
  println(c)

  println("--- OList ---")

  def g(s: String)(a: Int): VImpl[Option[Int]] = {
    Chc(tag(s), One(Some(a)), One(None))
  }

  var ol = ONil(): OList[Int]

  for(i <- 0 to 2) {
    ol = OCons(g("a" + i.toString)(10*(i+1)), ol)  
  }

  println(ol)
  println(ol.size())
  println(ol.get(0))
  println(ol.get(1))
  println(ol.get(2))

}

