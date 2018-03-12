package net.janvsmachine.catsbook.chapter4

import cats._
import cats.implicits._

object TreeMonad extends App {

  import net.janvsmachine.catsbook.chapter3.TreeExample._

  implicit def treeMonadInstance: Monad[Tree] = new Monad[Tree]() {
    override def pure[A](x: A) = Leaf(x)

    override def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]) = tree match {
      case Leaf(a) => f(a)
      case Branch(left, right) =>
        val l: Tree[B] = flatMap(left)(f)
        val r: Tree[B] = flatMap(right)(f)
        Branch(l, r)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(a) match {
      case Leaf(Left(x)) => tailRecM(x)(f)
      case Leaf(Right(y)) => Leaf(y)
      case Branch(l, r) => Branch(
        flatMap(l) {
          case Left(l2) => tailRecM(l2)(f)
          case Right(l2) => pure(l2)
        },
        flatMap(r) {
          case Left(r2) => tailRecM(r2)(f)
          case Right(r2) => pure(r2)
        }
      )
    }
  }


  def quote[A] = (x: A) => "'" + x.toString + "'"

  import cats.syntax.functor._

  val leaf1: Tree[Int] = Leaf(42)
  println(leaf1.map(quote))

  val leaf2: Tree[String] = Leaf("foo")
  println(leaf2.map(quote))

  val tree1: Tree[Int] = Branch(Leaf(42), Leaf(124))
  println(tree1.map(quote))

  def split(n: Int): Tree[Int] = if (n > 100) Branch(Leaf(n / 2), Leaf(n / 2)) else Leaf(n)

  val flatMapped = tree1.flatMap(split)

  println(s"Flatmapped tree: $flatMapped")

  assert(flatMapped == Branch(Leaf(42), Branch(Leaf(62), Leaf(62))))

}
