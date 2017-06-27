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
}
