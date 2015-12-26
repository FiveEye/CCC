package fe.CCC

class Lazy[T](func: => T) {
  var ret: Option[T] = None 
  
  def apply(): T = ret match {
    case Some(t) => t
    case None => { val tmp = {func}; ret = Some(tmp); tmp }
  }
  
  def isDone = ret match {
    case Some(t) => true
    case None => false
  }
  
  def map[U](f: T => U): Lazy[U] = Lazy(f(this()))
  
  def flatMap[U](f: T => Lazy[U]): Lazy[U] = f(this())
  
  override
  def toString = ret match {
    case Some(t) => t.toString
    case None => "lazy" //super.toString
  }
}

object Lazy {
  def apply[T](func : => T) = new Lazy[T](func)

  implicit def lazyTo[T](l: Lazy[T]): T = l()
}

