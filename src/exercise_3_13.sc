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

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(x, t) if f(x) => dropWhile(t)(f)
      case _ => l
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
    case _ => Nil
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  //
  // can we halt recursion if we see 1.0?
  // not possible, strict evolution done for f's arguments
  def product3_7(ns: List[Double]) =
    foldRight(ns, 1.0)((x,y) => x * y)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,acc) => 1 + acc)

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sum3_11(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3_11(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] =
  //  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B
  //    val z = List[a]
  //    val l: List[Int] = List[Int]()
  // val l: List[A] = List[A]() => z:B // empty list
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldRightByfoldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

}

val x = List(1, 2, 3, 4, 5, -6, 7, 8, 9)
val y1 = List.tail(x)
val y2 = List.setHead(9, y1)
val y3 = List.drop(x, 2)
val y4 = List.dropWhile(x)(x => x < 4)
// fails matching Nil
//val y5 = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
val y6 = List.length(x)
