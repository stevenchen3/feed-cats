package example.cats.fundamentals.herding.cats

import cats._
import cats.data._
import cats.implicits._

// A semigroup is any set `A` with an associative operation (`combine`).
// Associativity is the only law for `Semigroup`.
object SemigroupExample extends App {
  assert { (3 * 2) * (8 * 5) === 3 * (2 * (8 * 5)) }
  assert { List("la") ++ (List("di") ++ List("da")) === (List("la") ++ List("di")) ++ List("da") }

  // `combine` 's symbolic alias `|+|`
  val a = List(1, 2, 3) |+| List(4, 5, 6)
  println(s"a=${a}") // expect `List(1, 2, 3, 4, 5, 6)`

  val b = "one" |+| "two"
  println(s"b=$b") // expect `onetwo`

  def doSomething[A: Semigroup](a1: A, a2: A): A = a1 |+| a2
  val c = doSomething(3, 5)(Semigroup[Int])
  println(s"c=$c") // expect `8`

  val d = Semigroup[Int].combine(3, 5)
  println(s"d=$d") // expect `8`
}
