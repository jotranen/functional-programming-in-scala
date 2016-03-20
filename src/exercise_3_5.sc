import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def products(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * products(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil =>
        println("error")
        Nil
      case Cons(_, t) => t
    }
  }

  def setHead[A](n: A, l: List[A]): List[A] = {
    l match {
      case Nil =>
        println("error")
        Nil
      case Cons(_, t) => Cons(n, t)
    }
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else {
      l match {
        case Nil =>
          println("error")
          Nil
        case Cons(_, t) => drop(t, n-1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, t) => if (f(x)) dropWhile(t, f) else l
      case _ => l
    }
  }

}

val x = List(1, 2, 3, 4, 5, -1, 2)
val y1 = List.tail(x)
val y2 = List.setHead(9, y1)
val y3 = List.drop(x, 2)
val y4 = List.dropWhile(x, (a) => { if (a < 0) false else true })

