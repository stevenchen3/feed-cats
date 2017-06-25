package example.cats

import simulacrum._

// `Applicative` is actually a specific implementation of `Functor`
@typeclass trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def apply[A, B](fa: F[A])(ff: F[A ⇒ B]): F[B]

  // derived operation
  override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = apply(fa)(pure(f))
}

object Applicative {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)

    def apply[A, B](fa: Option[A])(ff: Option[A ⇒ B]): Option[B] =
      (fa, ff) match {
        case (None, _)          ⇒ None
        case (Some(a), None)    ⇒ None
        case (Some(a), Some(f)) ⇒ Some(f(a))
      }
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    def pure[A](a: A): List[A] = List(a)

    def apply[A, B](fa: List[A])(ff: List[A ⇒ B]): List[B] =
      for {
        a <- fa
        f <- ff
      } yield f(a)
  }
}

object ApplicativeApp extends App {
  import Applicative._
  val optApp = implicitly[Applicative[Option]]
  println(s"Applicative[Option].pure(a) = ${optApp.pure("a")}")
  println(s"Applicative[Option].map(Noe)(_ + 1) = ${optApp.map(None: Option[Int])(_ + 1)}")

  val listApp = implicitly[Applicative[List]]
  println(s"Applicative[List].pure(a) = ${listApp.pure("a")}")
  val res0 = listApp.apply(List(1, 2, 3))(List((x: Int) ⇒ (x + 1).toString))
  println(s"Applicative[List].apply(List(1, 2, 3))(List((x: Int) ⇒ (x + 1).toString)) = ${res0}")
  println(s"Applicative[List].map(List(1, 2, 3))(_ + 1) = ${listApp.map(List(1, 2, 3))(_ + 1)}")

}
