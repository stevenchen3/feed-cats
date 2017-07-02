package example.cats.contravariant

/**
  * The Contravariant type class is for functors that define a contramap function with the following
  * type:
  *
  * def contramap[A, B](fa: F[A])(f: B ⇒ A): F[B]
  *
  * Generally speaking, if you have some context F[A] for type A, and you can get an A value out of
  * a B value — Contravariant allows you to get the F[B] context for B.
  */
import cats._
import cats.functor._
import cats.implicits._

object ContravariantApp extends App {
  case class Money(amount: Int)
  case class Salary(size: Money)

  implicit val showMoney: Show[Money] = Show.show(m ⇒ s"$$${m.amount}")
  println(s"Money(100) = ${Money(100).show}")

  //implicit val showSalary: Show[Salary] = Show.show(s ⇒ s"$$${s.size.amount}")

  // If we want to show a Salary instance, we can just convert it to a Money instance and show it
  // instead.
  implicit val showSalary: Show[Salary] = showMoney.contramap(_.size)
  println(s"Salary(100) = ${Salary(Money(100)).show}")

  import scala.math.Ordered._
  // scala.math.Ordering type class defines comparison operations, e.g. compare:
  Ordering.Int.compare(2, 1) // 1
  Ordering.Int.compare(1, 2) // -1

  // There’s also a method, called by, that creates new Orderings out of existing ones:
  //
  // def by[T, S](f: T ⇒ S)(implicit ord: Ordering[S]): Ordering[T]
  //
  // In fact, it is just contramap, defined in a slightly different way! We supply T ⇒ S to receive
  // F[S] ⇒ F[T] back.
  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
  println(s"Money(100) < Money(200) = ${Money(100) < Money(200)}")

  // Subtyping
  class A
  class B extends A

  val b: B = new B
  val a: A = b

  implicit val showA: Show[A]  = Show.show(a ⇒ "a!")
  //implicit val showB1: Show[B] = showA.contramap(b ⇒ b: A)
  //implicit val showB2: Show[B] = showA.contramap(identity[A])

  // Subtyping relationships are "lifted" backwards by contravariant functors, such that if F is a
  // lawful contravariant functor and A <: B (A is a subtype of B) then F[B] <: F[A], which is
  // expressed by Contravariant.narrow.
  //
  // In this example, Show[B] <: Show[A], so:
  implicit val showB3: Show[B] = Contravariant[Show].narrow[A, B](showA)
  println(s"a.show = ${a.show}")
  println(s"b.show = ${b.show}")
}
