package net.janvsmachine.catsbook


object HomebrewMonads {

  trait Monad[F[_]] {

    def pure[A](value: A): F[A]

    def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  }

  implicit val optionMonadInstance: Monad[Option] = new Monad[Option] {
    override def pure[A](value: A) = Option(value)

    override def flatMap[A, B](value: Option[A])(f: A => Option[B]) = value.flatMap(f)
  }

  implicit val listMonadInstance: Monad[List] = new Monad[List] {
    override def pure[A](value: A) = List(value)

    override def flatMap[A, B](value: List[A])(f: A => List[B]) = value.flatMap(f)
  }

  trait MonadLaws[F[_]] {

    def leftIdentityLaw[A, B](a: A, f: A => F[B])(implicit m: Monad[F]): Boolean = {
      m.flatMap(m.pure(a))(f) == f(a)
    }

    def rightIdentityLaw[A](x: F[A])(implicit m: Monad[F]): Boolean = {
      m.flatMap(x)(m.pure) == x
    }

    def associativityLaw[A, B, C](x: F[A], f: A => F[B], g: B => F[C])(implicit m: Monad[F]): Boolean = {
      val l = m.flatMap(m.flatMap(x)(f))(g)
      val r = m.flatMap(x)(x => m.flatMap(f(x))(g))
      l == r
    }
  }

}

object MonadExampleRunner extends App {

  import HomebrewMonads._

  val list1 = List(1, 2, 3)
  val listMonad = implicitly[Monad[List]]
  val dupe = (x: Int) => List(x, x)
  val keepIfEven = (x: Int) => if (x % 2 == 0) List(x) else List.empty

  println(listMonad.flatMap(list1)(dupe))

  val listLaws = new MonadLaws[List] {}

  assert(listLaws.leftIdentityLaw(42, dupe))
  assert(listLaws.rightIdentityLaw(list1))
  assert(listLaws.associativityLaw(list1, dupe, keepIfEven))

  val optionMonad = implicitly[Monad[Option]]
  val incIfOdd: Int => Option[Int] = (x: Int) => if (x % 2 == 1) Option(x + 1) else None
  val doubleIfEven: Int => Option[Int] = (x: Int) => if (x % 2 == 0) Option(x * 2) else None

  println(optionMonad.flatMap(Option(42))(incIfOdd))
  println(optionMonad.flatMap(Option(43))(incIfOdd))
  println(optionMonad.flatMap(Option.empty[Int])(incIfOdd))

  val optionLaws = new MonadLaws[Option] {}

  assert(optionLaws.leftIdentityLaw(42, incIfOdd))
  assert(optionLaws.rightIdentityLaw(Option(42)))
  assert(optionLaws.rightIdentityLaw(Option.empty[Int]))

  assert(optionLaws.associativityLaw(Option(42), incIfOdd, doubleIfEven))

}
