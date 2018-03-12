package net.janvsmachine.catsbook.chapter3

import cats.Functor
import cats.instances.list._
import cats.instances.option._

object FunctorExamples extends App {

  // Footling around with cats Functors.

  val list1 = List(1, 3, 5, 7)

  val list2 = Functor[List].map(list1)(_ + 1)
  println(list2)

  val option1 = Option(42)

  val option2 = Functor[Option].map(option1)(_ * 2)
  println(option2)


  val inc = (x: Int) => x + 1

  val liftedInc = Functor[Option].lift(inc)

  val option1Inc = liftedInc(option1)
  println(option1Inc)

  val stringify = (x: Int) => s"'${x.toString}'"

  import cats.instances.function._
  import cats.syntax.functor._

  val stringifyInc = inc map stringify

  println(stringifyInc(42))

  def doSomeMath[F[_]](start: F[Int])(implicit fct: Functor[F]) =
    start.map(_ + 42)

  println(doSomeMath(Option(100)))

  println(doSomeMath(List(1, 2, 3)))

}
