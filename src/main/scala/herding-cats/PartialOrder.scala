package example.cats.fundamentals.herding.cats

import cats._
import cats.data._
import cats.implicits._

/**
  * `PartialOrder` enables `tryCompare` syntax which returns `Option[Int]`. According to algebra,
  * it'll return `None` if operands are not comparable.
  */
object PartialOrderExample extends App {
  println(s"1 tryCompare 2: ${1 tryCompare 2}")
  //println(s"1 tryCompare 2.0: ${1 tryCompare 2.0}") // compilation error: type mismatch
  println(s"1.0 tryCompare Double.NaN: ${1.0 tryCompare Double.NaN}")

  def lt[A: PartialOrder](x: A, y: A): Boolean = x <= y
  // lt: [A](x: A, y: A)(implicit evidence$1: cats.PartialOrder[A])Boolean

  //lt[Int](1, 2.0) // compilation error: type mismatch
  println(s"lt[Int](1, 2)=${lt[Int](1, 2)}")

  val res = 1 tryCompare 2 match {
    case Some(x) ⇒ s"$x"
    case None ⇒ "incomparable"
  }
  println(s"result=$res")
}
