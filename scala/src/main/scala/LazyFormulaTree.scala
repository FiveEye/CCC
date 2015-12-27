package fe.CCC

class LFTNode[A] {
  def map[B](f: A => B): LFTNode[B] = LFTNode.map(this, f)
  
  implicit def toLFT(): LazyFormulaTree[A] = LFTNode.toLFT[A](this)
}

object LFTNode {
  def map[A,B](v: LFTNode[A], f: A => B): LFTNode[B] = v match {
    case LFTChc(fm, y, n) => LFTChc(fm, Lazy(map(y, f)), Lazy(map(n, f)))
    case LFTOne(v) => LFTOne(Lazy(f(v())))
  }
  
  implicit def toLFT[A](l: LFTNode[A]): LazyFormulaTree[A] = new LazyFormulaTree(Lazy(l))
}

case class LFTChc[A](fm: Formula, y: Lazy[LFTNode[A]], n: Lazy[LFTNode[A]]) extends LFTNode[A]
case class LFTOne[A](v: Lazy[A]) extends LFTNode[A]

class LazyFormulaTree[A](val l: Lazy[LFTNode[A]]) extends V[LazyFormulaTree] {
  type C = A
  val vf = LazyFormulaTree  
  val va = this
}

object LazyFormulaTree extends VFactory[LazyFormulaTree] {
  def One[A](one: A): LazyFormulaTree[A] = new LazyFormulaTree(Lazy(LFTOne(Lazy(one))))
  def Chc[A](s: Formula, y: LazyFormulaTree[A], n: LazyFormulaTree[A]): LazyFormulaTree[A] = new LazyFormulaTree(Lazy(LFTChc(s, y.l, n.l)))
  
  override
  def map[A,B](v: LazyFormulaTree[A], f: A => B): LazyFormulaTree[B] = new LazyFormulaTree(Lazy(v.l().map(f)))
  
  def flatMap[A,B](va: LazyFormulaTree[A], f: (A)=>LazyFormulaTree[B]): LazyFormulaTree[B] = ???
  
  def isOne[A](t: LazyFormulaTree[A]): Option[A] = ???
  
  def evaluate[A](t: LazyFormulaTree[A], s: Set[String]): A = ???
  
  implicit def toLFTNode[A](x: LazyFormulaTree[A]): LFTNode[A] = x.l()
  
}

