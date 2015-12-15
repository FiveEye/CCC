import Prelude._

abstract class FormulaTree[A] extends V[FormulaTree] {
  type C = A
  val vf = FormulaTree
  val va = this
  //def map[B](f : A => B): FormulaTree[B] = FormulaTree.map(this,f)
  //def flatMap[B]( f: A=>FormulaTree[B]): FormulaTree[B] = FormulaTree.flatMap(this, f)
}

case class FTOne[A](v:A) extends FormulaTree[A]
case class FTChc[A](s: Formula, y: FormulaTree[A], n: FormulaTree[A]) extends FormulaTree[A]

object FormulaTree extends VFactory[FormulaTree] {
  def One[A](one: A) : FormulaTree[A] = FTOne(one)
  def Chc[A](s: Formula, y: FormulaTree[A], n: FormulaTree[A]): FormulaTree[A] = FTChc(s, y, n)
  
  override
  def map[A,B](v: FormulaTree[A], f: A => B): FormulaTree[B] = v match {
    case FTOne(v)     => FTOne(f(v))
    case FTChc(m,y,n) => FTChc(m, map(y,f), map(n,f))
  }

  def flatMap[A,B](v: FormulaTree[A], f: A=>FormulaTree[B]): FormulaTree[B] = v match {
    case FTOne(v)     => f(v)
    case FTChc(m,y,n) => FTChc(m, flatMap(y,f), flatMap(n,f))
  }
}
