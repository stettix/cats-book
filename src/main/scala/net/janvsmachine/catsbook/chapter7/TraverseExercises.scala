package net.janvsmachine.catsbook.chapter7

import cats.Applicative
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object TraverseExercises extends App {

  // Implementing traverse and sequence generically for Applicatives.

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F])((acc: F[List[B]], a: A) => (acc, f(a)).mapN(_ :+ _))

  def listSequence[F[_] : Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  // Example usage: combining Futures.

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

  val totalUptime = listTraverse(hostnames)(getUptime)
  Await.result(totalUptime, 1.second)


  // Example usage: combining lists.

  assert(listSequence(List(Vector(1, 2), Vector(3, 4))) == Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))

  println(listSequence(List(Vector(1, 2, 3), Vector(4, 5, 6))))


  // Combinging Option values

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  assert(process(List(2, 4, 6)) == Some(List(2, 4, 6)))
  assert(process(List(1, 2, 3)) == None)


  // Same as above but using Validated instead of Option

  import cats.data.Validated

  type ErrorsOr[A] = Validated[List[String], A]

  def validatedProcess(inputs: List[Int]): ErrorsOr[List[Int]] = listTraverse(inputs) { n =>
    if (n % 2 == 0) {
      Validated.valid(n)
    } else {
      Validated.invalid(List(s"$n is not even"))
    }
  }

  assert(validatedProcess(List(2, 4, 6)) == Validated.valid(List(2, 4, 6)))
  assert(validatedProcess(List(1, 2, 3)) == Validated.invalid(List("1 is not even", "3 is not even")))


  // Cats built-in Traverse, which is fully generic.

  import cats.Traverse

  val numbers = List(Future(1), Future(2), Future(3))
  val numbers2: Future[List[Int]] = Traverse[List].sequence(numbers)
  println("Future numbers: " + Await.result(numbers2, 1.second))

  // Using syntax

  assert(Await.result(hostnames.traverse(getUptime), 1.second) == List(1020, 960, 840))
  assert(Await.result(numbers.sequence, 1.second) == List(1, 2, 3))

}
