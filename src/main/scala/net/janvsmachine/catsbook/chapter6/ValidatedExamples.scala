package net.janvsmachine.catsbook.chapter6

import cats.Semigroupal
import cats.data.{NonEmptyVector, Validated}
import cats.instances.list._

object ValidatedExamples extends App {

  type AllErrorsOr[A] = Validated[List[String], A]

  // Combining values of Validated
  val res = Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )

  assert(res == Validated.invalid(List("Error 1", "Error 2")))

  // Different ways of creating valid and invalid values.

  // Creating directly via apply() method:
  val v: Validated[Nothing, Int] = Validated.Valid(123)
  val i: Validated[List[String], Nothing] = Validated.Invalid(List("Bad!"))

  // Creating via helper methods (these widen the return type to Validated):
  val vv: Validated[List[String], Int] = Validated.valid[List[String], Int](42)
  val ii: Validated[List[String], Int] = Validated.invalid[List[String], Int](List("Bad!"))

  // Via syntax/extension methods:
  import cats.syntax.validated._

  val vvv: Validated[List[String], Int] = 42.valid[List[String]]
  val iii: Validated[List[String], Int] = List("Bad!").invalid[Int]

  // Via pure and raiseError from applicative and applicativeError:
  {
    import cats.syntax.applicative._
    import cats.syntax.applicativeError._

    type ErrorsOr[A] = Validated[List[String], A]

    val valid: ErrorsOr[Int] = 123.pure[ErrorsOr]
    val invalid: ErrorsOr[Int] = List("Wrong!").raiseError[ErrorsOr, Int]
  }

  // Combine errors
  import cats.syntax.apply._
  import cats.syntax.validated._
  import cats.instances.vector._
  import cats.data.NonEmptyVector
  //import cats.implicits._

  val combined: Validated[NonEmptyVector[String], (Int, Int)] = (
    NonEmptyVector.of("Error 1").invalid[Int],
    NonEmptyVector.of("Error 2").invalid[Int]
  ).tupled

  val vx: Validated[String, Int] = "fail".invalid[Int]
  val vy: Validated[String, Int] = 42.valid[String]
  val folded: String = vy.fold(_ + "!!!", _.toString.reverse)

}
