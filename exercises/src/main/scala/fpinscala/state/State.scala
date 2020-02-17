package fpinscala.state

import fpinscala.state.RNG.{Simple, map2, nonNegativeInt, storage}
import javafx.beans.property.SimpleObjectProperty

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  def getDouble: (Double, RNG)
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

    override def getDouble: (Double, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toDouble // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  type testType = Int => Unit
  type storage = List[Int] => List[Double]

  val int: Rand[Int] = x => x.nextInt
  val doubleAlias: Rand[Double] = y => y.getDouble

  val ok: testType = Int => println("This works")
  val checkingThis: storage = _.map(_.toDouble)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nI, rn) = rng.nextInt
    if (nI < 0) ((nI + 1) * -1, rn)
    else (nI, rn)

  }
  def double1(rng: RNG): (Double, RNG) = {
    val (n, nr) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nr)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), ir) = intDouble(rng)
    ((d, i), ir)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val db1 = doubleAlias(rng)
    val db2 = doubleAlias(db1._2)
    val db3 = doubleAlias(db2._2)
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

  def doubleMap(rng: Rand[Int]): Rand[Double] = map(rng)(_.toDouble)

  def intDoubleMap(rng: Rand[Int], rng2: Rand[Double]): Rand[(Int, Double)] = {
    map2(rng, rng2)((x, y) => (x, y))
  }
  def doubleIntMap(rng: Rand[Double], rng2: Rand[Int]): Rand[(Double, Int)] = {
    map2(rng, rng2)((x, y) => (x, y))
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, nr) = rng.nextInt
    val (d, dr) = doubleAlias(nr)
    ((n, d), dr)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, rng1) = ra(rng)
        val (b, rng2) = rb(rng1)
        (f(a, b), rng2)
      }
  }
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    def processList(latestRange: RNG, fs: List[Rand[A]], newList: List[A]) = {
      fs match {
        case h :: tail => {
          val newRange1: (A, RNG) = h(latestRange);
          processList(newRange1._2, tail, List(newRange1._1))
        }
        case h :: Nil => {
          val newRange2: (A, RNG) = h(latestRange);
          processList(newRange2._2, Nil, List(newRange2._1))
        }
        case Nil => (newList, latestRange)
      }
    }
    val head: (A, RNG) = fs.head(rng)
    val body = fs.tail
    val combined = body.zipWithIndex.map {
      case (ra, idx) =>
        if (idx == 0) {
          ra(head._2)
        } else {
          ra(rng)
        }
    }
    (combined.map(f => f._1), rng)
  }

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

  //check stuff
  val firstCheck: storage = RNG.checkingThis
  val secondCheck: Seq[Double] = firstCheck(List(12, 12, 3, 12, 3))

  val getRandDouble: RNG.Rand[Double] = RNG.doubleAlias
  val tryAgain: (Double, RNG) = getRandDouble(Simple(10))
  val tryThat = tryAgain._1

  println(s"GOT THIS RANDOM DOUBLE $tryThat")
  val useUnit: RNG.Rand[Double] = RNG.unit(20.4)
  val randomUnit: (Double, RNG) = useUnit(Simple(12))
  val getTheVal = randomUnit._1
  println(s"GETTING THE VAL ===> $getTheVal")

  println(s"USING RANDOM ====>")
  val useMap: RNG.Rand[String] = RNG.map(RNG.int)(x => x.toString)
  val usingMap: (String, RNG) = useMap(Simple(21))
  val getMapVal = usingMap._1
  println(s"RANDOM NUMBER TO STRING $getMapVal")

  println(s"doubleMap ====>")
  val doubleMap: RNG.Rand[Double] = RNG.doubleMap(RNG.int)
  val doubleMapVal: (Double, RNG) = doubleMap(Simple(109))
  println(s"RANDOM NUMBER TO STRING ${doubleMapVal._1}")

  println("intDoubleMap ====>")
  val intDoubleMap: RNG.Rand[(Int, Double)] =
    RNG.intDoubleMap(RNG.int, RNG.doubleAlias)
  val intDoubleMapVal: ((Int, Double), RNG) = intDoubleMap(Simple(21323))
  println(s"RANDOM DOUBLE COMBINATOR ${intDoubleMapVal._1}")

  println("usingSequence ====>")
  val calSequence: RNG.Rand[List[Int]] =
    RNG.sequence(List(RNG.int, RNG.int, RNG.int))
  val sequenced: (List[Int], RNG) = calSequence(Simple(123))
  println(s"RANDOM SEQUENCE COMBINATOR ${sequenced._1}")
}
