package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

object FlatMapExample extends App {
  val x = (Right(3): Either[String, Int]) flatMap {x ⇒ Right(x + 1)}
  println(s"(Right(3): Either[String, Int]) flatMap {x ⇒ Right(x + 1)} = $x") // Some(4)

  val y = "wisdom".some map { _ + "!"}
  println(s"wisdom.some map { _ + !} = $y") // Some("wisdom!")

  val z = none[String] map { _ + "!"}
  println(s"none[String] map { _ + !} = $z") // None

  // `Apply.apply` 3 has been renamed to `Apply.ap`
  val x1 = ({(_: Int) + 3}.some) ap 3.some
  println(s"({(_: Int) + 3}.some) ap 3.some = $x1") // Some(6)

  val y1 = none[String ⇒ String] ap "greed".some
  println(s"none[String ⇒ String] ap greed.some = $y1") // None

  val z1 = ({(_: String).toInt}.some) ap none[String]
  println(s"({(_: String).toInt}.some) ap none[String] = $z1") // None

  val x2= 3.some flatMap {(x: Int) ⇒ (x + 1).some}
  println(s"3.some flatMap {(x: Int) ⇒ (x + 1).some} = $x2") // Some(4)

  val y2 = "smile".some flatMap { (x: String) ⇒  (x + " :)").some}
  println(s"smile.some flatMap { (x: String) ⇒  (x + :)).some} = $y2") // Some(smile :))
}
