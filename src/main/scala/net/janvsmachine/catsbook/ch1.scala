package net.janvsmachine.catsbook

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

object Runner extends App {

  import PrintableInstances._

  println(Printable.format("Foo"))
  println(Printable.format(42))
  // println(Printable.format(false)) // Won't compile!

  Printable.print("Bar")
  Printable.print("561")

}
