package fpinscala.datastructures

import fpinscala.datastructures.List.dropWhile

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)((x, y) => if (x == 0.0 || y == 0.0) 0.0 else x * y) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil        => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    val tail = List.tail(l)
    Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(List.tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _                    => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => List()
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = l match {
    case Nil        => 0
    case Cons(h, t) => 1 + length(t)
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil        => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def sum3[A](l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def product3[A](l: List[Int]): Int = {
    foldLeft(l, 1)((x, y) => x * y)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def append1[A](l: List[A], a: A) =
    foldRight(l, List(a): List[A])(Cons(_, _))

  def transform(l: List[Int]) = {
    map(l)(_ + 1)
  }

  def doubleToString(l: List[Double], n: List[String]): List[String] = {
    l match {
      case Nil        => n
      case Cons(h, t) => doubleToString(t, Cons(h.toString, n))
    }
  }

  def doubleToString2(l: List[Double]): List[String] = map(l)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil                 => as
    case Cons(h, t) if f(h)  => Cons(h, filter(t)(f))
    case Cons(h, t) if !f(h) => filter(t)(f)
  }



  def addTwoLists(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addTwoLists(t1, t2))
      case (_, _)                       => Nil
    }
  }

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = {
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case (Nil, Nil)                   => Nil
      case (_, _)                       => Nil
    }
  }

}

object main extends App {

  val g = (x: String, y: String) => x + y
  println(
    "ZIPPING WITH ====>" + List
      .zipWith(List("Hello", "Hi", "Hey"), List(" 1", " 2", " 3"))(
        (x: String, y: String) => x + y
      )
  )
}
