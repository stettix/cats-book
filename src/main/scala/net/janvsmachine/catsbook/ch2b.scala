package net.janvsmachine.catsbook

import cats.Monoid
import cats.instances.int._
import cats.instances.option._


object Ch2bRunner extends App {

  val a = Option(22)

  val b = Option(20)

  //  val c = Monoid[Option[Int]].combine(a, Option.empty[Int])
  val c = Monoid[Option[Int]].combine(a, b)

  println(c)

}
