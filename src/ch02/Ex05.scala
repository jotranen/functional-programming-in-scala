package ch02

class Ex05 extends App {
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
