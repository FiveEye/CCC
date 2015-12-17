package fe.CCC

class LazyFormulaList[A] extends V[LazyFormulaList] {
  type C = A
  val vf = LazyFormulaList  
  val va = this
}

case class LFLNil[A](val func: Lazy[A]) extends LazyFormulaList[A]
case class LFLCons[A](val fm: Formula, val func: Lazy[A], val next: Lazy[LazyFormulaList[A]]) extends LazyFormulaList[A]

object LazyFormulaList extends VFactory[LazyFormulaList] {
  def One[A](one: A): LazyFormulaList[A] = LFLNil(Lazy({one}))
  def Chc[A](m: Formula, y: LazyFormulaList[A], n: LazyFormulaList[A]): LazyFormulaList[A] = ???
  
  
  def lazyNil[A](one: => A): LazyFormulaList[A] = LFLNil(Lazy(one))
  
  def lazyCons[A](fm: Formula, func: => A, next: => LazyFormulaList[A]) = LFLCons(fm, Lazy(func), Lazy(next)) 

  
  def isOne[A](t: LazyFormulaList[A]): Option[A] = t match {
    case LFLNil(func) => Some(func())
    case LFLCons(fm, func, next) => None
  }
  
  def evaluate[A](t: LazyFormulaList[A], s: Set[String]): A = t match {
    case LFLNil(func) => func()
    case LFLCons(fm, func, next) => if(fm.evaluate(s)) func() else evaluate(next(), s)
  }
  
  override
  def map[A,B](v: LazyFormulaList[A], f: A => B): LazyFormulaList[B] = ???

  def flatMap[A,B](v: LazyFormulaList[A], f: A=>LazyFormulaList[B]): LazyFormulaList[B] = ???
    
}


