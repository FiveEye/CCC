package fe.CCC

import FormulaList.{One, Chc}

object Main extends App {
  type VImpl[A] = FormulaList[A]
  val V = FormulaList
  
  def f(s: String)(a: Int): VImpl[Int] = {
    Chc(tag(s), One(a + 1), One(a + 100))
  }

  //val s = List(1,2,3)
  //println(s :+ 4)
  //println(s.foldRight(0)((a:Int, b:Int) => a + b))
  
  println("--- FormulaList ---")
  var x = V.One(0)
  println(x)
  x = x.flatMap(f("A"))
  println(x)
  x = x.flatMap(f("B"))
  println(x)
  println(x.evaluate(Set()))
  println(x.evaluate(Set("A")))
  println(x.evaluate(Set("B")))
  println(x.evaluate(Set("A", "B")))
  //println((tag("a") & !tag("b")).evaluate(Set("a")))
  //println(Set(1,2,3) &~ Set(2,3,4))
  
  def genNat(n: Int): LazyFormulaList[Int] = {
    new LFLCons(tag(n.toString), Lazy({ n }), Lazy({genNat(n+1)}))
  }
  
  def genFib(n: Int, a: Int, b: Int): LazyFormulaList[Int] = {
    new LFLCons(tag(n.toString), Lazy({ a }), Lazy({genFib(n+1, b, a+b)}))
  }
  
  println("--- LazyFormulaList ---")
  val lazyFL = genFib(0,0,1)
  println(lazyFL.evaluate(Set("5")))
  println(lazyFL.evaluate(Set("6")))
  println(lazyFL.evaluate(Set("7")))
  println(lazyFL.evaluate(Set("8")))
  

}

