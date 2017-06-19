package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

/**
  * Instead of the standard `==`, Eq enables `===` and `=!=` syntax by declaring `eqv` method. The
  * main difference is that `===` would fail compilation if you tried to compare `Int` and `String`.
  *
  * A type class used to determine equality between 2 instances of the same
  * type. Any 2 instances `x` and `y` are equal if `eqv(x, y)` is `true`.
  * Moreover, `eqv` should form an equivalence relation.
  */
object EqExample extends App {
  println(s"1 === 1: ${1 === 1}")

  // 1 === "foo" // compilation error: type mismatch
  1 == "foo" // warning and always yield `false`

  val x = (Some(1): Option[Int]) =!= (Some(2): Option[Int])
  println(s"(Some(1): Option[Int]) =!= (Some(2): Option[Int]): ${x}")

  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
  object Color {
    def red: Color = Red
    def green: Color = Green
    def blue: Color = Blue
  }
  // https://github.com/typelevel/cats/blob/master/kernel/src/main/scala/cats/kernel/Eq.scala
  implicit val colorEq: Eq[Color] = new Eq[Color] {
    def eqv(x: Color, y: Color): Boolean = x == y
  }
  println(s"Color.red == Color.red: ${Color.red === Color.red}")
  println(s"Color.red == Color.green: ${Color.red === Color.green}")
  println(s"Color.gree == Color.blue: ${Color.green === Color.blue}")
}
