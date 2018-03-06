package net.janvsmachine.catsbook

object HomebrewMonoids {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit m: Monoid[A]) = m
  }

  object MonoidLaws {

    def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
      m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
    }

    def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
      (m.combine(x, m.empty) == x) &&
        (m.combine(m.empty, x) == x)
    }

  }

  object MonoidInstances {

    implicit val intAddInstance: Monoid[Int] = new Monoid[Int] {
      def empty = 0

      override def combine(x: Int, y: Int) = x + y
    }

    implicit val intMultiplicationInstance: Monoid[Int] = new Monoid[Int] {
      def empty = 1

      override def combine(x: Int, y: Int) = x * y
    }

  }

}

object MonoidRunner extends App {

  import net.janvsmachine.catsbook.HomebrewMonoids._

  //  import MonoidInstances.intAddInstance
  import MonoidInstances.intMultiplicationInstance

  val m = Monoid[Int]

  println(m.empty)

  println(m.combine(20, 22))

}
