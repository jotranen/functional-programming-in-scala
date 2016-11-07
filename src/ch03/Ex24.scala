package ch03

object Ex24 extends App {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
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
      case Cons(x, xs) if n > 0 => drop(xs, n - 1)
      case Cons(x, xs) => xs
    }

    def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
      as match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => as
      }

    def init[A](l: List[A]): List[A] = l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    // can we halt recursion if we see 1.0?
    // not possible, strict evaluation done for f's arguments
    def product3_7(ns: List[Double]) =
    foldRight(ns, 1.0)((x, y) => x * y)

    def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => 1 + b)

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }

    def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(as), z)((b, a) => f(a, b))

    def append[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2)(Cons(_,_))

    def concat[A](l: List[List[A]]): List[A] =
      foldRight(l, Nil:List[A])(append)

    def add1(l: List[Int]): List[Int] =
      foldRight(l, Nil:List[Int])((h, t) => Cons(h+1, t))

    def doubleListToStringList(l: List[Double]) : List[String] =
      foldRight(l, Nil:List[String])((h,t) => Cons(h.toString, t))

    def map[A,B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil:List[B])((h,t) => Cons(f(h), t))

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
      concat(map(as)(f))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if (f(a)) List(a) else Nil)

    def addLists(al: List[Int], bl: List[Int]): List[Int] =
      (al, bl) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case ((Cons(h1, t1), Cons(h2, t2))) => (Cons(h1+h2, addLists(t1, t2)))
      }

    def zipWith[A,B,C](al: List[A], bl: List[B])(f: (A,B) => C): List[C] =
      (al, bl) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case ((Cons(h1, t1), Cons(h2, t2))) => (Cons(f(h1,h2), zipWith(t1, t2)(f)))
      }

    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
      case (_,Nil) => true
      case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
      case _ => false
    }
    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(h,t) => hasSubsequence(t, sub)
    }
  }

  val l = List.addLists(List(1,2,3), List(4,5,6))
  println(l)
}
