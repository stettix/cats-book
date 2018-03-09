package net.janvsmachine.catsbook

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


object ch4d extends App {

  def slowly[A](op: => A): A =
    try op finally Thread.sleep(100)

  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] = for {
    ans <- if (n < 2) 1.pure[Logged] else slowly(factorial(n - 1).map(_ * n))
    _ <- Vector(s"Fact $n = $ans").tell
  } yield ans

  val res: Logged[Int] = factorial(5)
  val (logs, value) = res.run
  println(s"Logs = $logs, result = $value")

  val asyncRes: Seq[Logged[Int]] = Await.result(Future.sequence(Vector(
    Future(factorial(5)), Future(factorial(5))
  )), 10.seconds)

  asyncRes.foreach { r =>
    val (logs, value) = r.run
    println(s"Value: $value, logs: ${logs.mkString(", ")}")
  }

}
