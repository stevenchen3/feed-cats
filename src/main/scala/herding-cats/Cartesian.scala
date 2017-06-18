package example.cats.fundamentals.herding.cats

import cats._
import cats.data._
import cats.implicits._

object Cartesian extends App {
  // the type of hs will be
  // hs: List[Int ⇒ Int] = List(<function1>, <function1>, <function1>, <function1>)
  val hs = Functor[List].map(List(1, 2, 3, 4))({ (_: Int) * (_: Int) }.curried)
  val xs = Functor[List].map(hs) { _(9) }
  println(s"xs=${xs}")

  // Cartesian
  //
  // Cartesian defines product function, which produces a pair of (A, B) wrapped in effect F[_] out
  // of F[A] and F[B]. The symbolic alias for product is |@| also known as the applicative style.
  val x: Option[Int] = 9.some
  val y: Option[Int] = none[Int]
  println(s"x=${x}, y=${y}")

  // Option forms Cartesian
  val threeCrossFive = (3.some |@| 5.some) map { _ - _ }
  println(s"threeCrossFive=${threeCrossFive}") // expects res: Option[Int] = Some(-2)

  val threeCrossNone = (3.some |@| none[Int]) map { _ - _ } // expects: None
  println(s"threeCrossNone=${threeCrossNone}")

  // See Cartesian definition
  // https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/syntax/cartesian.scala
  //
  // List as a Cartesian
  val ys = (List("a", "b", "c") |@| List(1, 2, 3)) map { _ + _ }
  println(s"List |@| List = ${ys}")

  // > and < operators
  println(s"1.some <* 2.some = ${1.some <* 2.some}") // expects: Some(1)
  println(s"1.some *> 2.some = ${1.some *> 2.some}") // expects: Some(2)

  // If either side fails, we get None
  println(s"none[Int] <* 2.some = ${none[Int] <* 2.some}") // expects: None
  println(s"none[Int] *> 2.some = ${none[Int] *> 2.some}") // expects: None

  println(s"List(1, 2) <* List(a, b) = ${List(1, 2) <* List("a", "b")}") // expects: List(1, 1, 2, 2)
  println(s"List(1, 2) *> List(a, b) = ${List(1, 2) *> List("a", "b")}") // expects: List(a, b, a, b)
}
