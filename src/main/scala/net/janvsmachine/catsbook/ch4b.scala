package net.janvsmachine.catsbook

import cats.Monad
import cats.instances.option._
import cats.instances.list._

import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._


object ch4b extends App {

  val opt1 = Monad[Option].pure(42)

  val opt2 = Monad[Option].flatMap(opt1)(x => Option(x + 1))

  1.pure[Option]

  1.pure[List]

  def sumSquared[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y


  println("Sum squared: "  + sumSquared(Option(2), Option(3)))
  println(sumSquared(Option(3), Option.empty[Int]))

  // AND: play around with the Id monad in relation to this.

  import cats.Id
  println("Via Identity monad: " + sumSquared(2 : Id[Int], 3: Id[Int]))

  println("Via Identity monad with alternative syntax: " + sumSquared(Monad[Id].pure(2), Monad[Id].pure(3)))
}
