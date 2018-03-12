package net.janvsmachine.catsbook

import cats.data.{NonEmptyList, State}
import State._
import cats.syntax.applicative._

object ch4g extends App {

  type CalcState[A] = State[List[Int], A]

  // Simple, dumb version:
  def evalOneX(sym: String): CalcState[Int] = State[List[Int], Int] { oldStack =>
    (sym, oldStack) match {
      case ("+", first :: second :: rest) =>
        val res = first + second
        (res :: rest, res)
      case ("-", first :: second :: rest) =>
        val res = second - first
        (res :: rest, res)
      case ("*", first :: second :: rest) =>
        val res = first * second
        (res :: rest, res)
      case ("/", first :: second :: rest) =>
        val res = second / first
        (res :: rest, res)
      case (s, stack) =>
        val res = s.toInt
        (res :: stack, res)
    }
  }

  // Version composed out of two state transforms
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => evalOperator(_ + _)
    case "-" => evalOperator(_ - _)
    case "*" => evalOperator(_ * _)
    case "/" => evalOperator(_ / _)
    case n => evalOperand(n.toInt)
  }

  // Using a for comprehension
  def evalOperatorX(op: (Int, Int) => Int): CalcState[Int] = for {
    oldStack ← get[List[Int]]
    (res, remaining) = oldStack match {
      case first :: second :: rest =>
        (op(first, second), rest)
    }
    _ ← set[List[Int]](res :: remaining)
  } yield res

  // Using a simple function
  def evalOperator(op: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
    case first :: second :: rest =>
      val result = op(first, second)
      (result :: rest, result)
    case _ => throw new Exception("Stack has fewer than two values")
  }

  // Using for comprehension
  def evalOperandX(n: Int): CalcState[Int] = for {
    oldStack ← get[List[Int]]
    _ ← set[List[Int]](n :: oldStack)
  } yield n

  // Using a simple function
  def evalOperand(n: Int): CalcState[Int] = State[List[Int], Int] { oldStack =>
    (n :: oldStack, n)
  }

  println("Eval single int: " + evalOne("42").runA(Nil).value)
  println("Eval '+': " + evalOne("+").run(List(20, 22)).value)
  println("Eval '+' with bigger stack: " + evalOne("+").run(List(20, 22, 1, 2, 3)).value)

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    res <- evalOne("+")
  } yield res

  println("Result of program: " + program.runA(Nil).value)

  def evalAll(input: List[String]): CalcState[Int] = {
    val ops: List[CalcState[Int]] = input map evalOne

    ops.foldLeft(0.pure[CalcState]) { (agg, op) => agg.flatMap(_ => op) }
  }

  println("Eval all: " + evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value)

  def eval(input: String): Int = {
    val symbols = input.split("\\s").toList
    evalAll(symbols).runA(Nil).value
  }

  assert(eval("1 2 + 3 *") == 9)
}
