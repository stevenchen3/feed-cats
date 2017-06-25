package example.cats

/*
 *  Typeclass abstracts over type constructor that can define a `map` operation
 *  and comply with two laws: (1) identity law and (2) composition law.
 */
trait Functor[F[_]] { // requires type constructor argument
  def map[A, B](fa: F[A])(f: A ⇒ B): F[B]
}

trait FunctorLaws {
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) = F.map(fa)(a ⇒ a) == fa

  def composition[F[_], A, B, C](fa: F[A], f: A ⇒ B, g: B ⇒ C)(implicit F: Functor[F]) =
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
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
}
