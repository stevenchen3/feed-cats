package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

object ApplicativeExample extends App {
  println(s"Applicative[List].pure(1) = ${Applicative[List].pure(1)}") // List(1)
  println(s"Applicative[Option].pure(1) = ${Applicative[Option].pure(1)}") // Some(1)

  val F = Applicative[Option]
  val valx = F.ap(F.pure((_: Int) + 3))(F.pure(9))
  println(s"f.ap(f.pure((_: Int) + 3))(f.pure(9)) = ${valx}")

  import cats.syntax.cartesian._
  import cats.syntax.apply._
  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil ⇒ Applicative[F].pure(Nil: List[A])
    case x :: xs ⇒ (x |@| sequenceA(xs)) map { _ :: _ }
  }

  // Some(List(1, 2))
  println(s"sequenceA(List(1.some, 2.some)) = ${sequenceA(List(1.some, 2.some))}")

  val y = sequenceA(List(3.some, none[Int], 1.some))
  println(s"sequenceA(List(3.some, none[Int], 1.some)) = ${y}") // None

  val z = sequenceA(List(List(1, 2, 3), List(4, 5, 6)))
  println(s"sequenceA(List(List(1, 2, 3), List(4, 5, 6)))=${z}") // List[List[Int]]

  // Using sequenceA is useful when we have a list of functions and we want to feed the same input
  // to all of them and then view the list of results.
  //
  // Compiation error: Int ⇒ <error> takes no type parameters, expected: one
  //val f = sequenceA[Function1[Int, ?], Int](List((_: Int) + 3, (_: Int) + 2, (_: Int) + 1))
}
