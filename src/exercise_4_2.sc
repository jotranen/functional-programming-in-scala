import scala.{None, Either => _, Option => _, _}

case class Some2[+A](get: A) extends Option2[A]
case object None2 extends Option2[Nothing]

sealed trait Option2[+A] {

  def map[B](f: A => B): Option2[B] =
    this match {
      case None2 => None2
      case Some2(a) => Some2(f(a))
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case None2 => default
      case Some2(a) => a
    }

  def flatMap[B](f: A => Option2[B]): Option2[B] =
    map(f).getOrElse(None2)

  def orElse[B >: A](obj: => Option2[B]): Option2[B] =
    this map (Some2(_)) getOrElse(obj)

  def filter(f: A => Boolean): Option2[A] =
    this match {
      case Some2(a) if(a) => this
      case _ => None2
    }

  def mean(xs: Seq[Double]): Option2[Double] =
    if (xs.isEmpty) None2
    else Some2(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option2[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}

