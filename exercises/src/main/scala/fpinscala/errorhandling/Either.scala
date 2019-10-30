package fpinscala.errorhandling


import fpinscala.errorhandling.Option.sequence

import scala.{Either => _, Left => _, Option => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(v) => Right(f(v))
   case Left(e)  => Left(e)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match{
   case Right(v) => f(v)
   case Left(e)  => Left(e)

 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]  = this match{
   case Right(v) => Right(v)
   case _ => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for{
   a <- this
   bb <- b
 } yield f(a, bb)
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either extends App {

 println(Left("5").map(x => "Hello" + x))

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case h :: t =>
      f(h) flatMap { o =>
        traverse(t)(f).map(k => o :: k)
      }
    case _ => Right(Nil)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match {
    case h :: t =>
      h.flatMap { o =>
        sequence(t).map(k => o :: k)
      }
    case _ => Right(Nil)
  }


  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  println(traverse(List("1", "2l", "3", "4", "5"))(x => Try(x.toInt)))

}