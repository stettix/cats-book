package net.janvsmachine.catsbook

import cats.Functor
import cats.syntax.functor._

object ch3c extends App {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit val treeFunctorInstance: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](t: Tree[A])(f: A => B) = t match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left: Tree[A], right: Tree[A]) =>
        val l: Tree[B] = map(left)(f)
        val r: Tree[B] = map(right)(f)
        Branch(l, r)
    }
  }

  def quote[A] = (x: A) => "'" + x.toString + "'"

  val leaf1: Tree[Int] = Leaf(42)
  println(leaf1.map(quote))

  val leaf2: Tree[String] = Leaf("foo")
  println(leaf2.map(quote))


  val tree1: Tree[Int] = Branch(Leaf(42), Leaf(123))
  println(tree1.map(quote))

}
