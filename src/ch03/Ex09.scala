package ch03

object Ex09 extends App {
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
    def append[A](a1: List[A], a2: List[A]): List[A] = {
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }
    }
    def init[A](l: List[A]): List[A] = l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
    //
    // can we halt recursion if we see 1.0?
    // not possible, strict evaluation done for f's arguments
    def product3_7(ns: List[Double]) =
    foldRight(ns, 1.0)((x,y) => x * y)

    def length[A](as: List[A]): Int = foldRight(as, 0)((_,b) => 1 + b)
  }

  val res: List[Int] = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  val res2: List[Int] = List.foldRight(List(1,2,3), List(4,5,6))(Cons(_,_))

  println(res)
  println(res2)
}
