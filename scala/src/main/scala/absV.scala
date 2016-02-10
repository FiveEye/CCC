package fe.CCC

abstract class VFactory[_VImpl[A]] {
  type VImpl[A] = _VImpl[A]
  
  def One[A](one: A): VImpl[A]
  def Chc[A](s: Formula, y: VImpl[A], n: VImpl[A]): VImpl[A]
  
  def flatMap[A,B](va: VImpl[A], f: (A)=>VImpl[B]): VImpl[B]
  def map[A,B](va: VImpl[A], f: (A)=>B): VImpl[B] = flatMap(va, (a: A) => One(f(a)))
  
  def isOne[A](t: VImpl[A]): Option[A]
  def evaluate[A](t: VImpl[A], s: Set[String]): A
  
  def vToString[A](t: VImpl[A]): String = t.toString
  
}

abstract class V[VImpl[C] <: V[VImpl]] {
  type C
  def vf: VFactory[VImpl]
  def va: VImpl[C]
  
  def map[B](f: (C)=>B): VImpl[B] = vf.map(va, f)
  def flatMap[B](f: (C)=>VImpl[B]): VImpl[B] = vf.flatMap(va, f)
  
  def isOne(): Option[C] = vf.isOne(va)
  def evaluate(s: Set[String]): C = vf.evaluate(va, s)
  
  def apply(s: Set[String]): C = evaluate(s)
  
  override def toString: String = vf.vToString(va)
}
