package fpinscala.laziness

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import fpinscala.datastructures.{Cons => ListCons, Nil => ListNil}

trait Stream[+A] {

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => t().takeWhile(p)
    case _ => this
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty[A])(
      (a, b) => if (p(a)) {
        Stream.cons(a, b)
      } else {
        Stream.empty
      }
    )
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOptionFold: Option[A] =
    this.foldRight(None: Option[A])((a, b) => Some(a))

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // The arrow `=>` in front of the argument type `B` means that the function
  // `f` takes its second argument by name and may choose not to evaluate it.
  // If `f` doesn't evaluate its second argument, the recursion never occurs.

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](m: A => B): Stream[B] = {
    println("Mapping some elements")
    this.foldRight(Stream.empty[B])((a, b) => Stream.cons(m(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    println("Filtering some elements some elements")
    this.foldRight(Stream.empty[A])(
      (h, t) => if (f(h)) Stream.cons(h, t) else t
    )
  }

  def append[B](second: => Stream[B])(f: (A, Stream[B]) => Stream[B]): Stream[B] = {
    this.foldRight(second: Stream[B])((a, b) => f(a, b))
  }

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream extends App {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some(Tuple2(a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def fibInner(): Stream[Int] = {
    def fibRec(j: Int, k: Int): Stream[Int] = {
      Stream.cons(j, fibRec(k, j + k))
    }

    fibRec(0, 1)
  }

  def function(state: Int): Option[(Int, Int)] = {
    val newVal = (Math.random() * 100).toInt
    Some((newVal, state + newVal))
  }

  val fives = from(5)

  unfold(1)(function).take(10).toList


  val onesUnfold = unfold(1)(z => Some(1, z))
  val consUnfold = unfold(3)(z => Some(3, z))

  val fromUnfold = unfold(1)(z => Some(z, z + 1))

  val fibUnfold2 = unfold(ListBuffer(-1))(z =>
    z.toList match {
      case _ :: Nil => Some(0, z += 0)
      case _ :: _ :: Nil => Some(1, z += 1)
      case _ :: t => Some(t.last + t.reverse.tail.head, z += (t.last + t.reverse.tail.head))
    }
  )

  val fibUnfold3 = unfold(ListBuffer(-1))(z =>
    z.toList match {
      case _ :: Nil => Some(0, z += 0)
      case h :: ht :: Nil => Some(h+ht, z += 1)
      case _ :: t => Some(t.last + t.reverse.tail.head, z += (t.last + t.reverse.tail.head))
    }
  )

  println("FIB INNER " + fibInner().take(10).toList)
  println("FIB UNFOLD" + fibUnfold2.take(10).toList)

}
