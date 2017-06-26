package example.cats

import cats.Semigroup
import cats.implicits._

/**
  * If a type A can form a Semigroup it has an associative binary operation.
  *
  * ```
  * trait Semigroup[A] {
  *   def combine(x: A, y: A): A
  *   }
  * }
  * ```
  * Associativity means the following equality must hold for any choice of x, y, and z.
  *
  * ```
  * combine(x, combine(y, z)) = combine(combine(x, y), z)
  *
  * ```
  *
  */

object SemigroupApp extends App {

  implicit val intAdditionSemigroup: Semigroup[Int] = new Semigroup[Int] {
    def combine(x: Int, y: Int): Int = x + y
  }

  val x = 1
  val y = 2
  val z = 3
  Semigroup[Int].combine(x, y)
  val res1 = Semigroup[Int].combine(x, Semigroup[Int].combine(y, z))
  val res2 = Semigroup[Int].combine(Semigroup[Int].combine(x, y), z)
  assert(res1 == res2)

  // Infix syntax
  import cats.syntax.semigroup._
  println(s"1 |+| 2 = ${1 |+| 2}")

  // `Semigroup` for `Map`s
  import cats.instances.map._

  val map1 = Map("hello" -> 0, "world" -> 1)
  val map2 = Map("hello" -> 2, "cats"  -> 3)

  val res3 = Semigroup[Map[String, Int]].combine(map1, map2)
  val res4 = map1 |+| map2
  println(s"map1 |+| map2 = ${res4}")

  // Merging maps
  import cats.instances.all._
  import cats.syntax.semigroup._

  def optionCombine[A: Semigroup](a: A, opt: Option[A]): A =
    opt.map(a |+| _).getOrElse(a)

  def mergeMap[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
    lhs.foldLeft(rhs) {
      case (acc, (k, v)) ⇒ acc.updated(k, optionCombine(v, acc.get(k)))
    }

  val xm1 = Map('a' -> 1, 'b' -> 2)
  val xm2 = Map('b' -> 3, 'c' -> 4)
  println(s"mergeMap(xm1, xm2) = ${mergeMap(xm1, xm2)}")

  val ym1 = Map(1 -> List("hello"))
  val ym2 = Map(2 -> List("cats"), 1 -> List("world"))
  println(s"mergeMap(ym1, ym2) = ${mergeMap(ym1, ym2)}")

  // It is interesting to note that the type of mergeMap satisfies the type of Semigroup specialized
  // to Map[K, ?] and is associative.
  //Semigroup[Map[Char, Int]].combine(xm1, xm2) == mergeMap(xm1, xm2)
  //Semigroup[Map[Int, List[String]]].combine(ym1, ym2) == mergeMap(ym1, ym2)

  // Associativity laws
  assert(List(1, 2, 3).foldLeft(0)(_ |+| _) === List(1, 2, 3).foldRight(0)(_ |+| _))

  val list = List(1, 2, 3, 4, 5)
  val (left, right) = list.splitAt(2)
  val sumLeft = left.foldLeft(0)(_ |+| _)
  val sumRight = right.foldLeft(0)(_ |+| _)
  val result = sumLeft |+| sumRight // result: Int = 15
  println(s"result = ${result}")
}
