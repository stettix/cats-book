package net.janvsmachine.catsbook.chapter6

import cats.Semigroupal
import cats.instances.option._
import cats.syntax.apply._

object SemigroupalExamples extends App {

  val combined: Option[(Int, Int)] = Semigroupal[Option].product(Some(42), Some(123))

  assert(combined == Some(42, 123))

  val c2 = Semigroupal[Option].product(Some(42), None)

  assert(c2 == None)

  assert(Semigroupal.tuple3(Option(42), Option(123), Option(99)) == Some(42, 123, 99))
  assert(Semigroupal.tuple3(Option(42), None, Option(99)) == None)

  assert(Semigroupal.map3(Option(42), Option(123), Option(99))(_ + _ + _) == Some(42 + 123 + 99))
  assert(Semigroupal.map2(Option(42), None)(_ + _) == None)

  assert((Option(123), Option(42)).tupled == Some(123, 42))

  case class Cat(name: String, age: Int, color: String)

  assert((Option("Juno"), Option(2), Option("Grey")).mapN(Cat.apply) == Some(Cat("Juno", 2, "Grey")))
}
