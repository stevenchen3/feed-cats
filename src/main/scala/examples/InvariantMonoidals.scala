package example.cats.invariantmonoidal

import cats._
import cats.data._
import cats.implicits._

object InvariantMonoidalApp extends App {
  /**
    * To construct a Semigroup from a single value, we can define a trivial Semigroup with a combine
    * that always outputs the given value. A Semigroup[(A, B)] can be obtained from two Semigroups
    * for type A and B by deconstructing two pairs into elements of type A and B, combining these
    * element using their respective Semigroups, and reconstructing a pair from the results:
    */
  def pure[A](a: A): Semigroup[A] = new Semigroup[A] {
    def combine(x: A, y: A): A = a
  }

  def product[A, B](fa: Semigroup[A], fb: Semigroup[B]): Semigroup[(A, B)] =
    new Semigroup[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = (x, y) match {
        case ((xa, xb), (ya, yb)) ⇒ fa.combine(xa, ya) -> fb.combine(xb, yb)
      }
    }

  /**
    *
    * Given an instance of InvariantMonoidal for Semigroup, we are able to combine existing
    * Semigroup instances to form a new Semigroup by using the Cartesian syntax:
    */
  case class Foo(a: String, c: List[Double])
  implicit val fooSemigroup: Semigroup[Foo] = (
    (implicitly[Semigroup[String]] |@| implicitly[Semigroup[List[Double]]])
      .imap(Foo.apply)(Function.unlift(Foo.unapply))
  )

  val res1 = Foo("Hello", List(0.0)) |+| Foo("World", Nil) |+| Foo("!", List(1.1, 2.2))
  println(s"""Foo("Hello", List(0.0)) |+| Foo("World", Nil) |+| Foo("!", List(1.1, 2.2)) = $res1""")
}
