package ch02

import scala.annotation.tailrec

object Ex02 extends App {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) {
        true
      }
      else if (!ordered(as(n), as(n+1))) {
        false
      }
      else {
        loop(n+1)
      }
    }
    loop(0)
  }

  def ordered(a: Int, b: Int): Boolean = {
    if (a <= b) true
    else false
  }

  val a = Array(1,2,3,4,5)

  println(isSorted(a, ordered))

}
