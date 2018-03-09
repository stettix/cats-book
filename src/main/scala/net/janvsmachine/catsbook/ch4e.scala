package net.janvsmachine.catsbook

import cats.data.Reader
import cats.syntax.applicative._

//
// Reader Monad example exercise.
//

object ch4e extends App {

  case class Db(users: Map[Int, String], passwords: Map[String, String])

  type DbReader[T] = Reader[Db, T]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.users.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    username <- findUsername(userId)
    validPassword <- username.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
  } yield validPassword

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )
  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret")

  val db = Db(users, passwords)

  assert(checkLogin(1, "zerocool").run(db))
  assert(!checkLogin(2, "biff").run(db))
  assert(!checkLogin(4, "boff").run(db))

}
