/*
if a && b then 2
else if a || b then 101
else 200

list = {(a && b, 2), (a || b, 101)}
default = 200
*/

package fe.CCC

class FormulaList[A](val l:List[(Formula, A)], val d: A) extends V[FormulaList] {
  type C = A
  val vf = FormulaList  
  val va = this
}

object FormulaList extends VFactory[FormulaList] {
  def One[A](one: A): FormulaList[A] = new FormulaList[A](List(), one)
  
  def Chc[A](m: Formula, y: FormulaList[A], n: FormulaList[A]): FormulaList[A] = {
    val ll = y.l.map { case (ym, a) => (m & ym, a) }
    val lr = n.l.map { case (ym, a) => ((!m) & ym, a) }
    simplify(new FormulaList(ll ++ ((m, y.d) :: lr), n.d))
  }

  override
  def map[A,B](v: FormulaList[A], f: A => B): FormulaList[B] = new FormulaList( v.l.map { case (m, a) => (m, f(a)) }, f(v.d) )

  def flatMap[A,B](v: FormulaList[A], f: A=>FormulaList[B]): FormulaList[B] = {
    simplify(
      v.l.foldRight(f(v.d)) { case ((m, a), vb) => Chc(m, f(a), vb) }
    )
  }
  
  def isOne[A](t: FormulaList[A]): Option[A] = if (t.l.isEmpty) Some(t.d) else None 
  
  def evaluate[A](t: FormulaList[A], s: Set[String]): A = {
    t.l.find { case (m, a) => m.evaluate(s) } match {
      case Some((m, a)) => a
      case None         => t.d
    }
  }
  
  override
  def vToString[A](t: FormulaList[A]): String = {
    t.l.foldRight(t.d.toString) { case ((m,a), s) => "" + m + "->" + a + ", " + s }
    //t.l.foldRight("se " + t.d) { case ((m,a), s) => "if " + m + " " + a + "\nel" + s }
  }
  
  private def add[A](m: Formula, a: A, l:List[(Formula, A)]): List[(Formula, A)] = {
    if (l.isEmpty) {
      List((m, a))
    } else {
      val h = l.head
      if (h._2 == a) {
        (h._1 | m, h._2) :: l.tail
      } else {
        (m, a)::l
      }
    }
  }
  
  private def simplify[A](v: FormulaList[A]): FormulaList[A] = {
    val empty: List[(Formula, A)] = List()
    val w = v.l.foldLeft((True, empty)) {
      case ((lm, l), (m, a)) => {
        if ((m & lm) equivalentTo False) {
          (lm, l)
        } else {
          (lm & (!m), add(m, a, l))
        }
      }
    }
    
    new FormulaList(w._2.reverse, v.d)
  }
  
}

