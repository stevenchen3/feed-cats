package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

/*
 * See http://eed3si9n.com/herding-cats/Free-monoids.html
 *
 * # The definition of free monoid `M(A)` is given as follows:
 *
 * ## Universal Mapping Property of M(A)
 *
 * There is a function i: A ⇒ |M(A)|, and given any monoid N and any function f: A ⇒ |N|,
 * there is a unique monoid homomorphism f_hom = M(A) ⇒ N such that |f_hom| ∘ i = f
 *
 * Instead of A, we'll use X here. Also |N| means Set[N]. In terms of Scala:
 *
 * def i(x: X): Set[M[X]] = ???
 * def f(x: X): Set[N] = ???
 *
 * // there exists a unique
 * def f_hom(mx: M[X]): N
 *
 * // such that
 * def f_hom_set(smx: Set[M[X]]): Set[N] = smx map {f_hom}
 * f == f_hom_set compose i
 *
 * # Injective
 *
 * This intuitively shows that Set[M[X]] needs to be lossless for X to allow any f, meaning no two
 * values on X can map into the same value in M[X]. In algebra, this is expressed as i is injective
 * for arrows from Char.
 *
 * ## Definitions:
 *
 * An arrow f satisfying the property ‘for any pair of arrows x1: T ⇒ A and x2: T ⇒
 * A, if f ∘ x1 = f ∘ x2 then x1 = x2‘, it is said to be injective for arrows from T.
 *
 * ⇒ From Linear Algebra point of view (one-to-one mapping)
 *
 * There're value `x1` and `x2` from domain `T`, and a function `f` maps values from domain `T` to
 * domain `A`, if `f ∘ x1 = f ∘ x2` then `x1` = `x2`A, arrow from `T` is injective.
 */
object FreeMonoids {
  // In this example, `M[X]` is a set of characters, which is `String` (M[X] = String)
  // and `N = Int`
  def i(x: Char): Set[String] = Set(x.toString)
  def f(x: Char): Set[Int] = Set(x.toInt) // example only
  // `f_hom: String ⇒ Int`
  val f_hom: PartialFunction[String, Int] = { case mx: String if mx.size == 1 ⇒ mx.charAt(0).toInt }
  def f_hom_set(smx: Set[String]): Set[Int] = smx map {f_hom}
  val g = (f_hom_set _) compose (i _)
}
