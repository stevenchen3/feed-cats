package example.fsis

import simulacrum._

//
// Go through Functional Structures in Scala tutorials by Michael Pilquist
//
// `Applicative` is actually a specific implementation of `Functor`
// `Applicative` expects a type parameter of some type constructor, not proper type.
@typeclass trait Applicative[F[_]] extends Functor[F] { self ⇒
  // Example: take 1 and lift into List[Int](1)
  //
  // Called pure because it takes a raw, "pure" value that exists outside of
  // "effect system" and lifts into effect system. These are not side-effects,
  // rather that, for example, Option models the "effect" of having or not
  // having a value. List models the "effect" of having multiple values.
  def pure[A](a: A): F[A]

  // Takes two proper types, A and B, and an F[A], but instead of taking A ⇒ B,
  // as with Functor's map, takes a type that exists _within_ the type
  // constructor. Applicative operates inside the "container", Functor "unwraps"
  // and "rewraps".
  def apply[A, B](fa: F[A])(ff: F[A ⇒ B]): F[B]

  //
  // derived operations
  //
  def apply2[A, B, C](fa: F[A], fb: F[B])(ff: F[(A, B) ⇒ C]): F[C] =
    apply(fa)( apply(fb)(  map(ff)(f ⇒ b ⇒ a ⇒ f(a, b))  ) )

  override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = apply(fa)(pure(f))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) ⇒ Z): F[Z] =
    // by using `apply` to `fa`, we expect a function `F[A ⇒ Z]`
    // so that we can have `F[Z]` as the result
    // Now, we have `fb` and `f: (A, B) ⇒ Z`. If we `map` over `fb`
    // and partially apply function `f`: map(fb)(b ⇒ f(_, b)),
    // we have the function `A ⇒ Z`
    apply(fa)(map(fb)(b ⇒ f(_, b)))

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) ⇒ Z): F[Z] =
    // `map2` over `fb` and `fc`, can result in a function `g: A ⇒ Z`
    apply(fa)(map2(fb, fc)( (b, c) ⇒ {a ⇒ f(a, b, c)} )) // `map2` returns `F[A ⇒ Z]`

  // use divide and conquer approach
  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) ⇒ Z): F[Z] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) { case((a, b), (c, d)) ⇒ f(a, b, c, d) }

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((a, b) ⇒ (a, b))

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((a, b, c) ⇒ (a, b, c))

  def flip[A, B](ff: F[A ⇒ B]): F[A] ⇒ F[B] = fa ⇒ apply(fa)(ff)

  def compose[G[_]](implicit G: Applicative[G]): Applicative[Lambda[X ⇒ F[G[X]]]] =
    new Applicative[Lambda[X ⇒ F[G[X]]]] {
      def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))

      def apply[A, B](fga: F[G[A]])(ff: F[G[A ⇒ B]]): F[G[B]] = {
        // We have F[G[A]], so if we had a F[G[A] ⇒ G[B]], we could simply use
        // F's apply with that function. Thus we unpack `ff` to give us a function
        // gab: G[A ⇒ B] and call G.flip on it to yield G[A] ⇒ G[B]
        val x: F[G[A] ⇒ G[B]] = self.map(ff)(gab ⇒ G.flip(gab))
        self.apply(fga)(x)
      }
    }
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

// From https://github.com/prurph/fsis-scala/blob/master/src/main/scala/Applicative.scala
trait ApplicativeLaws[F[_]] {
  import Applicative.ops._
  import IsEq._

  implicit def F: Applicative[F]

  def applicativeIdentity[A](fa: F[A]) = fa.apply(F.pure((a: A) ⇒ a)) =?= fa

  // Result of lifting A and applying lifted A ⇒ B must match the result of
  // directly applying the A ⇒ B to A and _then_ lifting it into context.
  // Function application "distributes over" apply and pure.
  def applicativeHomomorphism[A, B](a: A, f: A ⇒ B) = F.pure(a).apply(F.pure(f)) =?= F.pure(f(a))

  // Lifting A and applying ff to it gives the same F[B] as lifting a function
  // A ⇒ B that returns f(a) and applying it to ff. In the second case we
  // lift an (A ⇒ B) ⇒ B, then apply it to F[A ⇒ B] to give F[B].
  def applicativeInterchage[A, B](a: A, ff: F[A ⇒ B]) =
    F.pure(a).apply(ff) =?= ff.apply(F.pure((f: A ⇒ B) ⇒ f(a)))

  // Map operation must be consistent with apply and pure.
  // Mapping over fa with pure function f must be equal to applying the a lifted f.
  def applicativeMap[A, B](fa: F[A], f: A ⇒ B) = fa.map(f) =?= fa.apply(F.pure(f))
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

  val res1 = Applicative[Option].map2(Some(1), Some(2))((x, y) ⇒ x + y)
  println(s"Applicative[Option].map2(Some(1), Some(2))(_ + _) = ${res1}") // expect `Some(3)`

  val res2 = Applicative[List].map2(List(1, 2, 3), List(4, 5, 6))(_ + _)
  println(s"Applicative[List].map2(List(1, 2, 3), List(4, 5, 6)) = ${res2}")

  val res3 = Applicative[List].map3(List(1, 2), List(3, 4), List(5, 6))(_ + _ + _)
  println(s"map3(List(1, 2), List(3, 4), List(5, 6))(_ + _ + _) = $res3")

  val res4 = Applicative[Option].tuple2(Some(1), Some(2)) // expects `Some((1, 2))`
  println(s"tuples(Some(1), Some(2)) = $res4")

  val res5 = Applicative[List].tuple2(List(1, 2), List(3, 4)) // permutation or cartesian product
  println(s"tuple2(List(1, 2), List(3, 4)) = ${res5}")

  val listCombineOpt = Applicative[List] compose Applicative[Option]
  val res6 = listCombineOpt.map2(List(Some(1), None, Some(2)), List(Some(1), Some(2)))(_ + _)
  println(s"Applicative[List] compose Applicative[Option] = ${res6}")
}
