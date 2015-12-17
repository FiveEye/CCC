package fe.CCC

class Lazy[T](func: => T) {
  var ret: Option[T] = None 
  
  def apply(): T = ret match {
    case None => { val tmp = {func}; ret = Some(tmp); tmp }
    case Some(t) => t
  }
  
  def isDone = ret match {
    case None => false
    case Some(t) => true
  }
  
  override
  def toString = ret match {
    case None => "lazy" //super.toString
    case Some(t) => t.toString
  }
}

object Lazy {
  def apply[T](func : => T) = new Lazy[T](func)

  implicit def lazyTo[T](l: Lazy[T]): T = l()
}

