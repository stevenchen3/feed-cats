package example.cats.fundamentals.herding.cats

import cats._
import cats.data._
import cats.functor._
import cats.implicits._

object Show {
  // Creates an instance of [[Show]] using the provided function
  def show[A](f: A ⇒ String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  // Creates an instance of [[Show]] using object toString
  def fromToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }

  implicit val catContravariantForShow: Contravariant[Show] =
    new Contravariant[Show] {
      def contramap[A, B](fa: Show[A])(f: B ⇒ A): Show[B] =
        show[B](fa.show _ compose f)
    }
}

object ShowExample extends App {
  /*
   * At first, it might seem silly to define Show because Scala already has toString on Any. Any
   * also means anything would match the criteria, so you lose type safety. The toString could be
   * junk supplied by some parent class:
   */
  println(s"3.show=${3.show}")
  println(s"(new {}).toString=${(new {}).toString}")
  //(new {}).show // compilation error

  case class Person(name: String)
  case class Car(model: String)

  implicit val personShow = Show.show[Person](_.name)
  println(s"Person(Alice).show=${Person("Alice").show}")

  implicit val carShow = Show.fromToString[Car]
  println(s"cats.Show[Car]=${cats.Show[Car]}")
}
