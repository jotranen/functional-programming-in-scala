package ch02

object Ex03 extends App {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => (b => f(a, b))
}
