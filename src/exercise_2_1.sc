import scala.annotation.tailrec

object Exercise_2_1 {
  def fib(n: Int): Int = {

    @tailrec
    def fib0(x1: Int, x2: Int, n: Int, cur: Int): Int = {
      if (cur >= n) {
        x2
      }
      else {
        fib0(x2, x1+x2, n, cur+1)
      }
    }

    fib0(0, 1, n, 1)
  }

  def main(args: Array[String]) : Unit  = {
    println("Exercise 2.1")
  }
}

Exercise_2_1.fib(6)
