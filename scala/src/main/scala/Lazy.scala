package fe.CCC

class Lazy[T](func: => T) {
  lazy val ret: T = { func }
  def apply() = ret
}

object Lazy {
  def apply[T](func : => T) = new Lazy[T](func)
}

