package fpinscala.datastructures
import scala.math._

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(r) + size(l)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n)      => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def map(t: Tree[Int])(f: Int => Int): Tree[Int] = t match {
    case Leaf(n)      => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

}

object Main extends App {

  val tree = Branch(Branch(Leaf(12), Branch(Leaf(3), Leaf(4))), Leaf(8))
  println("FINDING SIZE OF TREE ====>" + Tree.size(tree))
  val tree2 = Branch(Leaf(3), Leaf(5))
  val tree3 = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(10), Leaf(4)))

  println(s"FINDING TREE DEPTH ${Tree.depth(tree)}")

}
