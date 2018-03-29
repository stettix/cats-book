package net.janvsmachine.catsbook.chapter6

import cats.Monad
import cats.implicits._

object SemigroupalExercises extends App {

  // Implementing Semigroupal product in terms of flatMap
  def product[M[_] : Monad, A, B](a: M[A], b: M[B]): M[(A, B)] = {
    a.flatMap { (xa: A) =>
      b.map { (xb: B) =>
        xa -> xb
      }
    }
  }

  assert(product(Option(42), Option(123)) == Some(42 -> 123))
  assert(product(List(1, 2), List(3, 4)) == List(1 -> 3, 1 -> 4, 2 -> 3, 2 -> 4))

}
