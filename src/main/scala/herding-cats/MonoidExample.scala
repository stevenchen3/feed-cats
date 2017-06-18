package example.cats.fundamentals.herding.cats

import cats._
import cats.data._
import cats.implicits._

/**
  * A monoid is a semigroup with an identity. A monoid is a specialization of a
  * semigroup, so its operation must be associative. Additionally,
  * `combine(x, empty) == combine(empty, x) == x`. For example, if we have `Monoid[String]`,
  * with `combine` as string concatenation, then `empty = ""`.
  *
  * In addition to the semigroup law, monoid must satify two more laws:
  *
  * - associativity (x |+| y) |+| z = x |+| (y |+| z)
  * - left identity Monoid[A].empty |+| x = x
  * - right identity x |+| Monoid[A].empty = x
  */
class Disjunction(val unwrap: Boolean) extends AnyVal
object Disjunction {
  @inline def apply(b: Boolean): Disjunction = new Disjunction(b)
  implicit val disjunctionMonoid: Monoid[Disjunction] = new Monoid[Disjunction] {
    def combine(a1: Disjunction, a2: Disjunction): Disjunction =
      Disjunction(a1.unwrap || a2.unwrap)
    def empty: Disjunction = Disjunction(false)
  }
  implicit val disjunctionEq: Eq[Disjunction] = new Eq[Disjunction] {
    def eqv(a1: Disjunction, a2: Disjunction): Boolean =
      a1.unwrap == a2.unwrap
  }
}

class Conjunction(val unwrap: Boolean) extends AnyVal
object Conjunction {
  @inline def apply(b: Boolean): Conjunction = new Conjunction(b)
  implicit val conjunctionMonoid: Monoid[Conjunction] = new Monoid[Conjunction] {
    def combine(a1: Conjunction, a2: Conjunction): Conjunction =
      Conjunction(a1.unwrap && a2.unwrap)
    def empty: Conjunction = Conjunction(true)
  }
  implicit val conjunctionEq: Eq[Conjunction] = new Eq[Conjunction] {
    def eqv(a1: Conjunction, a2: Conjunction): Boolean =
      a1.unwrap == a2.unwrap
  }
}

object MonoidExample extends App {
  val x1 = Disjunction(true) |+| Disjunction(false)
  println(s"x1.unwrap=${x1.unwrap}")

  val x2 = Monoid[Disjunction].empty |+| Disjunction(true)
  println(s"x2.unwrap=${x2.unwrap}")

  val x3 = Conjunction(true) |+| Conjunction(false)
  println(s"x3.unwrap=${x3.unwrap}")

  val x4 = Monoid[Conjunction].empty |+| Conjunction(true)
  println(s"x4.unwrap=${x4.unwrap}")

  // One way is to treat `Maybe` a as a `monoid` only if its type parameter a is a monoid as well and
  // then implement mappend in such a way that it uses the mappend operation of the values that are
  // wrapped with Just.
  //
  implicit def optionMonoid[A](implicit ev: Semigroup[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def empty: Option[A] = None
      def combine(x: Option[A], y: Option[A]): Option[A] =
        x match {
          case None ⇒ y
          case Some(xx) ⇒
            y match {
              case None ⇒ x
              case Some(yy) ⇒ Some(ev.combine(xx, yy))
            }
        }
    }
  //val x5: Option[String] = none[String] |+| "andy".some
  //println(s"x5=${x5}") // expect `Some("andy")`

  // But if we don't know if the contents are monoids, we can't use mappend between them, so what
  // are we to do? Well, one thing we can do is to just discard the second value and keep the first
  // one. For this, the First a type exists.
  case class First[A: Eq](val unwrap: Option[A])
  object First {
    implicit def firstMonoid[A: Eq]: Monoid[First[A]] = new Monoid[First[A]] {
      def combine(a1: First[A], a2: First[A]): First[A] =
        First((a1.unwrap, a2.unwrap) match {
          case (Some(x), _) ⇒ Some(x)
          case (None, y) ⇒ y

        })
      def empty: First[A] = First(None: Option[A])

    }
    implicit def firstEq[A: Eq]: Eq[First[A]] = new Eq[First[A]] {
      def eqv(a1: First[A], a2: First[A]): Boolean =
        Eq[Option[A]].eqv(a1.unwrap, a2.unwrap)
    }
  }
  val x6 = First('a'.some) |+| First('b'.some)
  println(s"x6=$x6")
  val x7 = First(none[Char]) |+| First('b'.some)
  println(s"x7=$x7")

  // f we want a monoid on Maybe a such that the second parameter is kept if both parameters of
  // mappend are Just values, Data.Monoid provides a the Last a type.
  case class Last[A: Eq](val unwrap: Option[A])
  object Last {
    implicit def lastMonoid[A: Eq]: Monoid[Last[A]] = new Monoid[Last[A]] {
      def combine(a1: Last[A], a2: Last[A]): Last[A] =
        Last((a1.unwrap, a2.unwrap) match {
          case (_, Some(y)) ⇒ Some(y)
          case (x, None) ⇒ x

        })
      def empty: Last[A] = Last(None: Option[A])

    }
    implicit def lastEq[A: Eq]: Eq[Last[A]] = new Eq[Last[A]] {
      def eqv(a1: Last[A], a2: Last[A]): Boolean =
        Eq[Option[A]].eqv(a1.unwrap, a2.unwrap)
    }
  }
  val x8 = Last('a'.some) |+| Last('b'.some)
  println(s"x8=$x8")

  val x9 = Last('a'.some) |+| Last(none[Char])
  println(s"x9=$x9")
}
