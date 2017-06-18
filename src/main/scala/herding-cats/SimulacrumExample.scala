package example.cats.fundamentals.herding.cats

import cats._
import cats.data._
import cats.implicits._
import simulacrum._

@typeclass
trait CanTruthy[A] { self ⇒

  /** Return true, if `a` is truthy. */
  def truthy(a: A): Boolean
}

object CanTruthy {
  def fromTruthy[A](f: A ⇒ Boolean): CanTruthy[A] = new CanTruthy[A] {
    def truthy(a: A): Boolean = f(a)
  }
}

@typeclass
trait CanAppend[A] {
  @op("|+|") def append(a: A, b: A): A
}

object SimulacrumExample extends App {
  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.fromTruthy({
    case 0 ⇒ false
    case _ ⇒ true
  })

  import CanTruthy.ops._
  println(s"10.CanTruthy=${10.truthy}")

  implicit val intCanAppend: CanAppend[Int] = new CanAppend[Int] {
    def append(a: Int, b: Int): Int = a + b
  }
  println(s"1 |+| 2 = ${1 |+| 2}")
}
