package net.janvsmachine.catsbook

object HomebrewFunctors extends App {

  trait Functor[F[_]] {

    def map[A, B](fa: F[A])(f: A => B): F[B]

  }

  trait FunctorLaws[F[_]] {

    def identityLaw[A](x: F[A])(implicit fct: Functor[F]): Boolean = {
      fct.map(x)(identity) == x
    }

    def distributionLaw[A, B, C](x: F[A], f: A => B, g: B => C)(implicit fct: Functor[F]): Boolean = {
      fct.map(x)(f andThen g) == fct.map(fct.map(x)(f))(g)
    }

  }

  implicit val optionFunctorInstance: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

//  implicit def fnFunctorInstance[X]: Functor[Function] = new Functor[Function] {
  //    def map[A, B](f: X => A)(g: A => B): X => B = f andThen g
  //  }

  val x = new FunctorLaws[Option] {}

  assert(x.identityLaw(Option(42)))

  val add1: Int => Int = (n: Int) => n + 1
  val mul2: Int => Int = (n: Int) => n * 2
  assert(x.distributionLaw(Option(42), add1, mul2))
}

