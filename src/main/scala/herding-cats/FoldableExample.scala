package example.cats.fundamentals.herding.cats

import cats._
import cats.data._
import cats.implicits._

object FoldableExample extends App {
  val x1 = Foldable[List].foldLeft(List(1, 2, 3), 1) { _ * _ } // expect `6`
  println(s"x1=$x1")

  val x2 = Foldable[List].fold(List(1, 2, 3))(Monoid[Int])
  println(s"x2=$x2") // expect `6`

  val x3 = List(1, 2, 3).foldMap(identity)(Monoid[Int])
  println(s"x3=$x3") // expect `6`

  val x4 = List(true, false, true) foldMap { Conjunction(_) }
  println(s"x4=${x4.unwrap}")
}
