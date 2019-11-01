package fpinscala.errorhandling

sealed trait Partial[+A, +B] {

  def map2[EE >: A, C, D](b: Partial[EE, C])(f: (B, C) => D): Partial[EE, D] = for{
    a <- this
    bb <- b
  } yield f(a, bb)


  def map[C](f: B => C): Partial[A, C] = this match {
    case Success(v) => Success(f(v))
    case Errors(e)  => Errors(e)
  }

  def flatMap[EE >: A, C](f: B => Partial[EE, C]): Partial[EE, C] = this match{
    case Success(v) => f(v)
    case Errors(e)  => Errors(e)

  }
//  case class Left[+E](get: E) extends Either[E,Nothing]
//  case class Right[+A](get: A) extends Either[Nothing,A]
}

case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]

case class Success[+B](get: B) extends Partial[Nothing, B]

object Partial extends App {

  case class Person(name: Name, age: Age)


  sealed class Name(val value: String)


  sealed class Age(val value: Int)


  def mkName(name: String): Partial[String, Name] = {
    if (name == "" || name == null) Errors(Seq("Name is empty.")) else Success(new Name(name))

  }

  def mkAge(age: Int): Partial[String, Age] = {
    if (age < 0) Errors(Seq("Age is out of range.")) else Success(new Age(age))
  }

  def mkPerson(name: String, age: Int): Partial[String, Person] = mkName(name).map2(mkAge(age))(Person(_, _))
  println(mkPerson("", -1))
  println(mkAge(1).map(x => println(x.value)))

}