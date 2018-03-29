package net.janvsmachine.catsbook.chapter5

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.flatMap._

import scala.concurrent.Await

//import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object MonadTransformerExercise extends App {

  type ExplicitResponse[A] = Future[Either[String, A]]

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(a) => EitherT.right(Future(a))
      case None => EitherT.left(Future(s"Comms error: $autobot unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      powerLevel1 <- getPowerLevel(ally1)
      powerLevel2 <- getPowerLevel(ally2)
    } yield powerLevel1 + powerLevel2 > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val futureReport: Response[String] = for {
      canMove <- canSpecialMove(ally1, ally2)
      report = if (canMove) s"$ally1 and $ally2 are ready to roll out!" else s"$ally1 and $ally2 need a recharge."
    } yield report

    val report: Either[String, String] = Await.result(futureReport.value, 10.seconds)

    report match {
      case Left(error) => error
      case Right(reportContent) => reportContent
    }
  }

  assert(tacticalReport("Jazz", "Bumblebee") == "Jazz and Bumblebee need a recharge.")
  assert(tacticalReport("Bumblebee", "Hot Rod") == "Bumblebee and Hot Rod are ready to roll out!")
  assert(tacticalReport("Jazz", "Ironhide") == "Comms error: Ironhide unreachable")

}
