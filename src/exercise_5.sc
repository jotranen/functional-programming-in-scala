import Stream._

trait Stream[+A] {
  def toList[A]: List[A] = {
    def toList0(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => toList0(t(), h() :: acc)
      case _ => acc

    }
    toList0(this, List()).reverse
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}