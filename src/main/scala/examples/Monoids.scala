package example.cats.monoid

import cats.Monoid
/**
 * Monoid extends the power of Semigroup by providing an additional empty value.
 * trait Semigroup[A] {
 *   def combine(x: A, y: A): A
 * }
 *
 * trait Monoid[A] extends Semigroup[A] {
 *   def empty: A
 * }
 *
 */

package ex1 {
  import cats.Semigroup
  final case class NonEmptyList[A](head: A, tail: List[A]) {
    def ++ (other: NonEmptyList[A]): NonEmptyList[A] = NonEmptyList(head, tail ++ other.toList)

    def toList: List[A] = head :: tail
  }

  object NonEmptyList {
    implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] =
      new Semigroup[NonEmptyList[A]] {
        def combine(x: NonEmptyList[A], y: NonEmptyList[A]): NonEmptyList[A] = x ++ y
      }
  }
}

object MonoidApp extends App {

  implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  val x = 1
  val res1 = Monoid[Int].combine(x, Monoid[Int].empty)
  println(s"Monoid[Int].combine(x, Monoid[Int].empty) = ${res1}")
  val res2 = Monoid[Int].combine(Monoid[Int].empty, x)
  assert(res1 == res2)

  def combineAll[A: Monoid](xs: List[A]): A = xs.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  import cats.instances.all._
  val res3 = combineAll(List(1, 2, 3))
  println(s"combineAll(List(1, 2, 3)) = ${res3}")

  val res4 = combineAll(List("hello", "world"))
  println(s"combineAll(List(hello, world)) = ${res4}")

  val res5 = combineAll(List(Map('a' -> 1), Map('a' -> 2, 'b' -> 3), Map('b' -> 4, 'c' -> 5)))
  println(s"combineAll(list of map) = $res5")

  val res6 = combineAll(List(Set(1, 2), Set(2, 3, 4, 5)))
  println(s"combineAll(list of set) = ${res6}")

  import cats.Semigroup
  import cats.syntax.semigroup._
  // Monoid for Option: for any Semigroup[A], there is a Monoid[Option[A]]
  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def empty: Option[A] = None

    def combine(x: Option[A], y: Option[A]): Option[A] = x match {
      case None ⇒ y
      case Some(xv) ⇒ y match {
        case None ⇒ x
        case Some(yv) ⇒ Some(xv |+| yv)
      }
    }
  }

  import cats.Monoid
  import cats.data.NonEmptyList
  import cats.instances.option._

  val list = List(NonEmptyList(1, List(2, 3)), NonEmptyList(4, List(5, 6)))
  val lifted = list.map(nel => Option(nel))

  // This lifting and combining of Semigroups into Option is provided by Cats as
  // Semigroup.combineAllOption.
  println(s"Monoid.combineAll(lifted) = ${Monoid.combineAll(lifted)}")
}
