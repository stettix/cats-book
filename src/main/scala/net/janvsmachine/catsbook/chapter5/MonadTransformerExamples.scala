package net.janvsmachine.catsbook.chapter5

import cats.data.OptionT

import cats.instances.list._
import cats.syntax.applicative._

object MonadTransformerExamples extends App {

  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]

  val comb1: ListOption[Int] = result1.flatMap { (x: Int) =>
    result2.map { (y: Int) =>
      x + y
    }
  }

  println(comb1.value)
  assert(comb1 == OptionT(List(Option(42))))

  val comb2: ListOption[Int] = for {
    x <- result1
    y <- result2
  } yield x + y

  assert(comb2.value == List(Option(42)))

}
