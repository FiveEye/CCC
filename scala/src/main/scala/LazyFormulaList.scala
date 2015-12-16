import Prelude._

class LazyFormulaList[A] extends V[LazyFormulaList] {
  type C = A
  val vf = LazyFormulaList  
  val va = this
}

case class LFLNil[A](val func: () => A) extends LazyFormulaList[A]
case class LFLCons[A](val s: Formula, val func: () => A, val next: () => LazyFormulaList[A]) extends LazyFormulaList[A]

object LazyFormulaList extends VFactory[LazyFormulaList] {
  def One[A](one: A): LazyFormulaList[A] = LFLNil(() => one)
  
  def Chc[A](m: Formula, y: LazyFormulaList[A], n: LazyFormulaList[A]): LazyFormulaList[A] = ???
  
  override
  def map[A,B](v: LazyFormulaList[A], f: A => B): LazyFormulaList[B] = ???

  def flatMap[A,B](v: LazyFormulaList[A], f: A=>LazyFormulaList[B]): LazyFormulaList[B] = ???
    
}


