package example.herding.cats

import cats._
import cats.data._
import cats.data.{NonEmptyList ⇒ NEL}
import cats.implicits._

import Validated.{valid, invalid}

object ValidatedDataType extends App {
  val v1 = valid[String, String]("event 1 ok")
  println(s"v1 = $v1")

  val v2 = invalid[String, String]("event 2 ok")
  println(s"v2 = $v2")

  // The difference about Validation is that it is does not form a monad, but forms an applicative
  // functor. Instead of chaining the result from first event to the next, Validated validates all
  // events:
  val result = (valid[String, String]("event 1 ok") |@|
    invalid[String, String]("event 2 failed!;") |@|
    invalid[String, String]("event 3 failed!;")) map {_ + _ + _}
  println(s"chaining result = ${result}")

  // NonEmptyList
  println(s"NEL.of(1) = ${NEL.of(1)}")
  val result2 = (valid[NEL[String], String]("event 1 ok") |@|
    invalid[NEL[String], String](NEL.of("event 2 failed!")) |@|
    invalid[NEL[String], String](NEL.of("event 3 failed!"))) map {_ + _ + _}

  // Extract values using `fold`
  val errs: NEL[String] = result2.fold({l ⇒ l}, {r ⇒ sys.error("invalid is expected")})
  println(s"errors = ${errs}")
}
