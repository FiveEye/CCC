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
  def Chc[A](m: Formula, y: LazyFormulaList[A], n: LazyFormulaList[A]): LazyFormulaList[A] = y match {
    case LFLCons(fm, func, next) => LFLCons(m & fm, func, Lazy(Chc(m, next(), n)))
    case LFLNil(func) => LFLCons(m, func, Lazy(fmMap(n, (fm: Formula) => (!m) & fm)))
  }
  
  def LazyChc[A](m: Formula, y: Lazy[LazyFormulaList[A]], n: Lazy[LazyFormulaList[A]]): Lazy[LazyFormulaList[A]] = y() match {
    case LFLCons(fm, func, next) => Lazy(LFLCons(m & fm, func, LazyChc(m, next, n)))
    case LFLNil(func) => Lazy(LFLCons(m, func, Lazy(fmMap(n(), (fm: Formula) => (!m) & fm))))
  }
  
  
  def LazyNil[A](one: => A): LazyFormulaList[A] = LFLNil(Lazy(one))
  def LazyCons[A](fm: Formula, func: => A, next: => LazyFormulaList[A]) = LFLCons(fm, Lazy(func), Lazy(next)) 
  
  def fmMap[A](v: LazyFormulaList[A], f: Formula => Formula): LazyFormulaList[A] = v match {
    case LFLCons(fm, func, next) => LFLCons(f(fm), func, next.map((x: LazyFormulaList[A]) => fmMap(x, f)))
    case LFLNil(func) => LFLNil(func)
  }
  
  
  override
  def map[A,B](v: LazyFormulaList[A], f: A => B): LazyFormulaList[B] = v match {
    case LFLCons(fm, func, next) => LazyCons(fm, {f(func())}, next.map((x: LazyFormulaList[A]) => map(x, f)))
    case LFLNil(func) => LazyNil({f(func())})
  }

  def flatMap[A,B](v: LazyFormulaList[A], f: A=>LazyFormulaList[B]): LazyFormulaList[B] = v match {
    case LFLCons(fm, func, next) => { val _y = Lazy(f(func())); val _n = Lazy(flatMap(next(), f)); LazyChc(fm, _y, _n)}
    case LFLNil(func) => f(func())
  }
  
  def isOne[A](t: LazyFormulaList[A]): Option[A] = t match {
    case LFLCons(fm, func, next) => None
    case LFLNil(func) => Some(func())
  }
  
  def evaluate[A](t: LazyFormulaList[A], s: Set[String]): A = t match {
    case LFLCons(fm, func, next) => if(fm.evaluate(s)) func() else evaluate(next(), s)
    case LFLNil(func) => func()
  }
  
  override
  def vToString[A](t: LazyFormulaList[A]): String = t match {
    case LFLCons(fm, func, next) => { "" + fm + "->" + func + ", " + next }
    case LFLNil(func) => func.toString
  }
    
}


