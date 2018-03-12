package net.janvsmachine.catsbook.chapter4

import cats.data.State
import cats.data.State._

object StateMonad extends App {

  // Noodling around with state monads.

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  print(program.run(10).value)
}
