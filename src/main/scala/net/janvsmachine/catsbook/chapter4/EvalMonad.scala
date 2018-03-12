package net.janvsmachine.catsbook.chapter4

import cats.Eval

object EvalMonad extends App {

  // Playing around with the Eval monad.

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
    case head :: tail =>
      val recursiveResult: Eval[B] = Eval.defer(foldRight(tail, acc)(fn))
      recursiveResult.map(res => fn(head, res))
    case Nil =>
      Eval.now(acc)
  }

  println(foldRight((1 to 50000).toList, 0)(_ + _).value)
}
