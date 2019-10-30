package fpinscala.errorhandling

import scala.util.Try
import scala.{
  Either => _,
  Option => _,
  Some => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case _       => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None    => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case _       => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case Some(v) => Some(v)
      case None    => ob
    }
  }

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(v => if (f(v)) Some(v) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option extends App {

  Some(3).getOrElse("Hello World")
  None.orElse(Some(1000))
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(ax => b.map(bx => f(ax, bx)))

  def map2For[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for{
    aa <- a
    bb <- b
  }yield f(aa, bb)


  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case h :: t =>
        h.flatMap { o =>
          sequence(t).map(k => o :: k)
        }
      case _ => Some(Nil)
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case h :: t =>
        f(h) flatMap { o =>
          traverse(t)(f).map(k => o :: k)
        }
      case _ => Some(Nil)
    }

  println(
    sequence(List(Some(2), None, Some(3), Some(4), Some(5), Some(7), Some(9)))
  )
  def toInt(s: String) = {
    Try(s.toInt)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

}
