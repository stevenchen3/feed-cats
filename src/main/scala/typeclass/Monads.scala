package example.typeclass

import simulacrum._

// `Monad` is actually a subtype of `Applicative`
@typeclass trait Monad[F[_]] extends Applicative[F] { self ⇒
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B]

  // consider `X ⇒ Y` as the `A` in `flatMap`, so we can apply
  // `flatMap` over `ff`. Each element in `ff` will be `X ⇒ Y`
  override def apply[X, Y](fx: F[X])(ff: F[X ⇒ Y]): F[Y] =
    flatMap(ff)((f: X ⇒ Y) ⇒ map(fx)(f))

  // another implementation of `map`
  override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] =
    flatMap(fa)(a ⇒ pure(f(a)))

  //
  // derived functions
  //
  // Turn a list of list into a list
  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity) // equivalent to: flatMap(ffa)(fa ⇒ fa)
}

object Monad {
  implicit val listMonad: Monad[List] = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)

    def flatMap[A, B](fa: List[A])(f: A ⇒ List[B]): List[B] = fa.flatMap(f)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](a: A): Option[A] = Option(a)

    def flatMap[A, B](fa: Option[A])(f: A ⇒ Option[B]): Option[B] = fa.flatMap(f)
  }
}

trait MonadLaws[F[_]] {
  import Monad.ops._
  import IsEq._

  implicit def F: Monad[F]

  def flatMapAssociativity[A, B, C](fa: F[A], f: A ⇒ F[B], g: B ⇒ F[C]): IsEq[F[C]] =
    fa.flatMap(f).flatMap(g) =?= fa.flatMap{ a ⇒ f(a).flatMap { b ⇒ g(b) } }

  def leftIdentify[A, B](a: A, f: A ⇒ F[B]): IsEq[F[B]] = F.pure(a).flatMap(f) =?= f(a)

  def rightIdentity[A](fa: F[A]): IsEq[F[A]] = fa.flatMap(a ⇒ F.pure(a)) =?= fa
}

object MonadLaws {
  def apply[F[_]](implicit F0: Monad[F]): MonadLaws[F] = new MonadLaws[F] {
    def F = F0
  }
}
