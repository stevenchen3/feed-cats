package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

/**
  * Given some type constructor C[_] and two types A and B, we want to apply functions of
  * type C[A]⇒C[B]. Unfortunately we have only functions of types A⇒B,  A⇒C[B] and C[A⇒B] at
  * hand, so we need adequate transformations for them:
  *
  * a) (A⇒B)    ⇒ (C[A]⇒C[B])
  * b) (A⇒C[B]) ⇒ (C[A]⇒C[B])
  * c) (C[A⇒B]) ⇒ (C[A]⇒C[B])
  *
  * All these are transformations of some given functions into the function type we need, and all
  * these transformations even have names.
  *
  * a) (A⇒B)    ⇒ (C[A]⇒C[B])  | Functor
  * b) (A⇒C[B]) ⇒ (C[A]⇒C[B])  | Monad
  * c) (C[A⇒B]) ⇒ (C[A]⇒C[B])  | Applicative
  *
  */
object FunctorsExample extends App {
  val x = Functor[List].map(List(1, 2, 3, 4)) { _ + 1 }
  println(s"x=$x")

  // Either as functor
  // Cats defines a Functor instance for Either[A, B]
  // It only works because Either[A, B] at the moment does not implement its own map.
  val y = (Right(1): Either[String, Int]) map { _ + 1 }
  println(s"y=$y")

  val z = (Left("boom!"): Either[String, Int]) map { _ + 1 }
  println(s"z=$z")

  // Function as functor
  // Cats also defines a Functor instance for Function1.
  val h = ((x: Int) ⇒ x + 1) map { _ * 7 }
  println(s"h(3)=${h(3)}")

  // It's not RIGHT in Scala
  val xx = (((_: Int) * 3) map { _ + 100 })(1) // ((x: Int) = x * 3) map {_ + 100} (1)
  println(s"xx=${xx}")
  val yy = (((x: Int) ⇒ x * 3) map { _ + 100 })(1)
  println(s"yy=${yy}")

  // Lifting a function
  val lifted = Functor[List].lift { (_: Int) * 2 }
  println(s"lifted(List(1, 2, 3))=${lifted(List(1, 2, 3))}")

  // `Functor` laws
  //
  // The FIRST functor law states that if we map the id function over a functor, the functor that we
  // get back should be the same as the original functor.
  val r: Either[String, Int] = Right(1)
  assert { (r map identity) === r }

  // The SECOND law says that composing two functions and then mapping the resulting function over a
  // functor should be the same as first mapping one function over the functor and then mapping the
  // other one.
  val f = { (_: Int) * 3 }
  val g = { (_: Int) + 1 }
  assert { (x map (f map g)) === (x map f map g) }
}
