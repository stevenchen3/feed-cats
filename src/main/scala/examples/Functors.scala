package example.cats.functor

import cats.Functor
import cats.instances.list._
import cats.instances.option._

object FunctorApp extends App {
  val listOption = List(Some(1), Some(2), None, Some(3))
  val res1 = Functor[List].compose[Option].map(listOption)(_ + 1)
  println(s"Functor[List].compose[Option].map(listOption)(_ + 1) = ${res1}")

  // Above approach can introduce complications in more complex use cases. For example, if we need
  // to call another function which requires a Functor and we want to use the composed Functor, we
  // would have to explicitly pass in the composed instance during the function call or create a
  // local implicit.
  def needsFunctor[F[_]: Functor, A](fa: F[A]): F[Unit] = Functor[F].map(fa)(_ => ())

  def foo: List[Option[Unit]] = {
    val listOptionFunctor = Functor[List].compose[Option]
    type ListOption[A] = List[Option[A]]
    needsFunctor[ListOption, Int](listOption)(listOptionFunctor)
  }

  // make this nicer at the cost of boxing with the Nested data type
  import cats.data.Nested
  import cats.syntax.functor._
  val nested: Nested[List, Option, Int] = Nested(listOption)
  println(s"nested.map(_ + 1) = ${nested.map(_ + 1)}")
}
