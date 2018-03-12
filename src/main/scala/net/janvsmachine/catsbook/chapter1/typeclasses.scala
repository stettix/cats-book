package net.janvsmachine.catsbook.chapter1

trait Printable[A] {

  def format(value: A): String

}

object PrintableInstances {

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int) = value.toString
  }

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String) = value.toString
  }

}

object Printable {

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)


  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(p.format(value))
}

final case class Cat(name: String, age: Int, colour: String)

object Cat {

  import PrintableInstances._

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(cat: Cat) = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val colour = Printable.format(cat.colour)
      s"$name is a $age year-old $colour cat"
    }
  }
}

object PrintableSyntax {

  implicit class PrintableOps[A](underlying: A) {
    def format(implicit p: Printable[A]): String = Printable.format(underlying)

    def print()(implicit p: Printable[A]): Unit = Printable.print(underlying)
  }

}

object Runner extends App {

  import PrintableInstances._

  println(Printable.format("Foo"))
  println(Printable.format(42))
  // println(Printable.format(false)) // Won't compile!

  Printable.print("Bar")
  Printable.print("561")

  val juno = Cat("Juno", 3, "gray")

  import PrintableSyntax._

  juno.print()
  println(juno.format)

  import cats._
  import cats.implicits._

  implicit val catsShowable: Show[Cat] = Show.show((cat: Cat) =>
    s"${cat.name.show} is a ${cat.age.show} year-old ${cat.colour.show} cat"
  )

  println(juno.show)

}
