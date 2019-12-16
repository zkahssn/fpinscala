package fpinscala.state

import fpinscala.state.RNG.Simple
import javafx.beans.property.SimpleObjectProperty


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nI, rn) = rng.nextInt
    if (nI < 0) ((nI + 1) * -1, rn)
    else (nI, rn)

  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nr) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nr)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, nr) = rng.nextInt
    val (d, dr) = double(nr)
    ((n, d), dr)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), ir) = intDouble(rng)
    ((d, i), ir)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val db1 = double(rng)
    val db2 = double(db1._2)
    val db3 = double(db2._2)
    ((db1._1, db2._1, db3._1), db3._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val next = rng.nextInt
    count match {
      case -1 => (List(), next._2)
      case n => {
        val next1 = ints(count - 1)(next._2)
        (next._1 :: next1._1, next1._2)
      }
    }
  }


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State extends App {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  val (num, rn2): (Int, RNG) = Simple(45).nextInt

  val test: (Int, RNG) = RNG.nonNegativeInt(rn2)
  println("NON NEGATIVE 2 => " + test)

  println("NON NEGATIVE 3=> " + RNG.ints(10)(rn2))


}
