import Prelude._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.sat._


abstract class VFactory[VImpl[A]] {
  def One[A](one: A): VImpl[A]
  def Chc[A](s: Formula, y: VImpl[A], n: VImpl[A]): VImpl[A]
  def flatMap[A,B](va: VImpl[A], f: (A)=>VImpl[B]): VImpl[B]
  def map[A,B](va: VImpl[A], f: (A)=>B): VImpl[B] = flatMap(va, (a: A) => One(f(a)))
}

abstract class V[VImpl[C] <: V[VImpl]] {
  type C
  def vf: VFactory[VImpl]
  def va: VImpl[C]
  
  def map[B](f: (C)=>B): VImpl[B] = vf.map(va, f)
  def flatMap[B](f: (C)=>VImpl[B]): VImpl[B] = vf.flatMap(va, f)
}
