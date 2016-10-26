package ch03

object Ex05 extends App {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => xs
    }
    def setHead[A](l: List[A], head: A): List[A] = l match {
      case Cons(_, xs) => Cons(head, xs)
      case _ => List(head)
    }
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) if n > 0 => drop(xs, n-1)
      case Cons(x, xs) => xs
    }
    def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
      as match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => as
    }
  }

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4,_))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  println(x) // x + y, 1 + 2 = 3
}
