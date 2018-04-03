package net.janvsmachine.catsbook.chapter6

import cats.data.Validated
import cats.syntax.either._
import cats.syntax.apply._
import cats.instances.list._


object ValidatedExercise extends App {

  case class User(name: String, age: Int)

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]


  def getValue(field: String)(data: FormData): FailFast[String] =
    Either.fromOption[List[String], String](data.get(field), List(s"Missing parameter '$field'"))

  val getName: FormData => FailFast[String] = getValue("name")

  def parseInt(fieldName: String, str: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](str.toInt)
      .leftMap(_ => List(s"Not a valid value for '$fieldName': '$str'"))

  def getAge(params: FormData) =
    getValue("age")(params).right.flatMap(x => parseInt("age", x))

  def nonBlank(str: String): Boolean = str.trim.nonEmpty

  def nonNegative(value: Int): Boolean = value >= 0

  def readName(params: FormData): FailFast[String] = getName(params).ensure(List("Name must not be blank"))(nonBlank)

  def readAge(params: FormData): FailFast[Int] = getAge(params).ensure(List("Age must be a positive integer"))(nonNegative)

  def readUser(params: FormData): FailSlow[User] = (
    readName(params).toValidated,
    readAge(params).toValidated
  ).mapN(User.apply)

  // Some ad-hoc tests.

  val validParams: FormData = Map("name" -> "Bob", "age" -> "42")
  val invalidParams: FormData = Map("foo" -> "?", "age" -> "wat")
  val badlyFormedParams: FormData = Map("name" -> "", "age" -> "-1")

  assert(getValue("name")(validParams) == Right("Bob"))
  assert(getValue("X")(validParams) == Left(List("Missing parameter 'X'")))

  assert(getName(validParams) == Right("Bob"))
  assert(getAge(validParams) == Right(42))

  assert(getName(invalidParams) == Left(List("Missing parameter 'name'")))
  assert(getAge(invalidParams) == Left(List("Not a valid value for 'age': 'wat'")))

  assert(readName(badlyFormedParams) == Left(List("Name must not be blank")))
  assert(readAge(badlyFormedParams) == Left(List("Age must be a positive integer")))

  assert(readUser(validParams) == Validated.valid(User("Bob", 42)))
  assert(readUser(invalidParams) == Validated.invalid(
    List("Missing parameter 'name'", "Not a valid value for 'age': 'wat'")))
  assert(readUser(badlyFormedParams) == Validated.invalid(
    List("Name must not be blank", "Age must be a positive integer")
  ))

}
