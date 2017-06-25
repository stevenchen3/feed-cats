package example.fsis

import simulacrum._

//
// Go through Functional Structures in Scala tutorials by Michael Pilquist
//
/*
 *  Typeclass abstracts over type constructor that can define a `map` operation
 *  and comply with two laws: (1) identity law and (2) composition law.
 */
// requires type constructor argument
@typeclass trait Functor[F[_]] { self ⇒
  def map[A, B](fa: F[A])(f: A ⇒ B): F[B]

  // Some derived operations for functor
  def lift[A, B](f: A ⇒ B): F[A] ⇒ F[B] = fa ⇒ map(fa)(f)

  def as[A, B](fa: F[A], b: ⇒ B): F[B] = map(fa)(_ ⇒ b)

  def void[A](fa: F[A]): F[Unit] = as(fa, ())

  def compose[G[_]](implicit G: Functor[G]): Functor[Lambda[X ⇒ F[G[X]]]] =
    new Functor[Lambda[X ⇒ F[G[X]]]] {
      def map[A, B](fga: F[G[A]])(f: A ⇒ B): F[G[B]] =
        self.map(fga)(ga ⇒ G.map(ga)(a ⇒ f(a)))
    }
}

object Functor {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A ⇒ B): List[B] = fa.map(f)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A ⇒ B): Option[B] = fa.map(f)
  }

  // Functor for non-collection
  // Define a functor for `function1` that maps the result of over a function.
  // We still get a function takes A as input but changes the type of output
  implicit def function1Functor[X]: Functor[X ⇒ ?] = new Functor[X ⇒ ?] {
    def map[A, B](fa: X ⇒ A)(fb: A ⇒ B): X ⇒ B = fa andThen fb
  }
}

trait FunctorLaws[F[_]] {
  import Functor.ops._
  import IsEq._

  // Compiler complains that `F[_]` shadows `F[_]` in `FunctorLaws`
  //def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) = F.map(fa)(a ⇒ a) =?= fa
  def identity[A](fa: F[A])(implicit F: Functor[F]) = F.map(fa)(a ⇒ a) =?= fa

  def composition[A, B, C](fa: F[A], f: A ⇒ B, g: B ⇒ C)(implicit F: Functor[F]) =
    F.map(F.map(fa)(f))(g) =?= F.map(fa)(f andThen g)
}


object FunctorLaws {
  def apply[F[_]](implicit F0: Functor[F]): FunctorLaws[F] = new FunctorLaws[F] {
    def F = F0
  }
}

object FunctorApp extends App {
  import Functor._
  val listF = implicitly[Functor[List]]
  println(s"listF.map(List(1, 2, 3)(x ⇒ x + 1)) = ${listF.map(List(1, 2, 3))(_ + 1)}")

  val optionF = implicitly[Functor[Option]]
  println(s"optionF.map(Some(1))(x ⇒ x + 1) = ${optionF.map(Some(1))(_ + 1)}")

  // `fun1IntFunctor` maps `function1` over another function,
  // but it still take argument Int as input
  val fun1IntFunctor = implicitly[Functor[Int ⇒ ?]]
  val f = fun1IntFunctor.map(x ⇒ x + 1)(y ⇒ y + 2)
  println(s"f(5)=${f(5)}")

  // After adding annotation `@typeclass`, we can do the following
  println(s"Functor[List].map(List(1, 2, 3))(_ + 1) = ${Functor[List].map(List(1, 2, 3))(_ + 1)}")

  // From `simulacrum`
  import Functor.ops._
  println(s"List(1, 2, 3) = ${List(1, 2, 3).void}")
  println(s"List(1, 2, 3).as(10) = ${List(1, 2, 3).as(10)}")
  val liftToString = Functor[List].lift((x: Int) ⇒ x.toString)(List(1, 2, 3))
  println(s"Functor[List].lift((x: Int) ⇒ x.toString)(List(1, 2, 3)) = ${liftToString}")

  val lisOptF = Functor[List] compose Functor[Option]
  val xs: List[Option[Int]] = List(Some(1), None, Some(3))
  println(s"lisOptF.map(xs)(_ + 1) = ${lisOptF.map(xs)(_ + 1)}") // List(Some(2), None, Some(4))

  // Something interesting about anomynous function
  val y = Functor[({type l[a] = Function1[Int, a]})#l]
  println(s"y = $y")
  // Scala compiler plugin `kind-projector` enables the question mark support
  // below statements are equivalent
  Functor[Function1[Int, ?]]
  Functor[Lambda[X ⇒ Function1[Int, X]]] // type function with `Lambda` keyword
}
