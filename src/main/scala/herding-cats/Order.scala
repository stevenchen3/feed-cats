package example.cats.fundamentals.herding.cats

import cats._
import cats.data._
import cats.implicits._

/**
  * `Ord` is for types that have an ordering. `Ord` covers all the standard comparing functions such
  * as >, <, >= and <=.
  *
  * Cats' equivalent for the `Ord` typeclass is `Order`
  */
object OrderExample extends App {
  println(s"1 > 2.0: ${1 > 2.0}") // yield false
  //println(s"1 compare 2.0: ${1 compare 2.0}") // compilation error, type mismatch
  println(s"1.0 compare 2.0: ${1.0 compare 2.0}")
  println(s"1.0 max 2.0: ${1.0 max 2.0}")
  println(s"Chen compare Wong: ${"Chen" compare "Wong"}")

  sealed trait Person {
    def first: String
    def last: String
  }
  case class Employee(val first: String, val last: String, salary: Float) extends Person

  // See
  // https://github.com/typelevel/cats/blob/master/kernel/src/main/scala/cats/kernel/Order.scala
  implicit val employeeOrder: Order[Person] = new Order[Person] {
    def compare(x: Person, y: Person): Int = {
      val z = x.first compare y.first
      z match {
        case 0 ⇒ x.last compare y.last
        case _ ⇒ z
      }
    }
  }

  val alice: Person =
    new Employee(last = "Chen", first = "Alice", salary = 10000)
  val bob: Person = new Employee(last = "Chen", first = "Bob", salary = 10000)
  val charlie: Person =
    new Employee(last = "Wong", first = "Alice", salary = 10000)
  println(s"alice compare charlie: ${alice compare charlie}")
  println(s"alice compare alice: ${alice compare alice}")
}
