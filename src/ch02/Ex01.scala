package ch02

import scala.annotation.tailrec

object Ex01 extends App {
  def fibonacci(n: Int): Int = {

    @tailrec
    def loop(x1: Int, x2: Int, cur: Int): Int = {
      if (cur >= n) {
        x2
      }
      else {
        loop(x2, x1+x2, cur+1)
      }
    }

    loop(0, 1, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    s"The ${name} of ${n} is ${f(n)}"
  }

  println(formatResult("Fibonacci", 6, fibonacci))
}

