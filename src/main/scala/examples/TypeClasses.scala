package example.cats

/*
 *
 * Type classes are a powerful tool used in functional programming to enable ad-hoc polymorphism,
 * more commonly known as overloading. Where many object-oriented languages leverage subtyping for
 * polymorphic code, functional programming tends towards a combination of parametric polymorphism
 * (think type parameters, like Java generics) and ad-hoc polymorphism.
 *
 * Monoid Laws:
 * Monoid type class requires that combine be associative and empty be an identity element for
 * combine.
 *
 * - combine(x, combine(y, z)) = combine(combine(x, y), z)
 * - combine(x, id) = combine(id, x) = x
 *
 */
trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

// For this reason, many libraries that provide type classes provide a utility method on the
// companion object of the type class, usually under the name apply, that skirts the need to call
// implicitly everywhere.
object Monoid {
  def apply[A : Monoid]: Monoid[A] = implicitly[Monoid[A]]
}

package ex1 {
  object MonoidExample1 {
    final case class Pair[A, B](first: A, second: B)

    // Implicit derivation
    def deriveMonoidPair[A, B](A: Monoid[A], B: Monoid[B]): Monoid[Pair[A, B]] =
      new Monoid[Pair[A, B]] {
        def empty: Pair[A, B] = Pair(A.empty, B.empty)

        def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
          Pair(A.combine(x.first, y.first), B.combine(x.second, y.second))
      }
  }
}

package ex2 {
  final case class Pair[A, B](first: A, second: B)
}

object Demo { // needed for tut, irrelevant to demonstration
  final case class Pair[A, B](first: A, second: B)

  object Pair {
    implicit def tuple2Instance[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[Pair[A, B]] =
      new Monoid[Pair[A, B]] {
        def empty: Pair[A, B] = Pair(A.empty, B.empty)

        def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
          Pair(A.combine(x.first, y.first), B.combine(x.second, y.second))
      }
  }
}

object TypeClasses extends App {
  // collapse a list, sum a list of integers, concate a list of strings, and union a list of sets
  def sumInts(list: List[Int]): Int = list.foldRight(0)(_ + _)

  def concatStrings(list: List[String]): String = list.foldRight("")(_ ++ _)

  def unionSets[A](list: List[Set[A]]): Set[A] = list.foldRight(Set.empty[A])(_ union _)

  // something in common among above three: all take an initial value and a combining function

  implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }

  // If list is empty, we have no values to work with and therefore can’t get the empty value. Not
  // to mention the oddity of getting a constant value from a non-static object.
  def combineAll[A](list: List[A], a: Monoid[A]): A = list.foldRight(a.empty)(a.combine)
  val xs = List(1, 2, 3, 4)
  assert(sumInts(xs) == combineAll(xs, intAdditionMonoid))

  // Subtyping
  def combineAll2[A <: Monoid[A]](list: List[A]): A = ???

  def combineAll3[A](list: List[A])(implicit A: Monoid[A]): A = list.foldRight(A.empty)(A.combine)

  // use syntactic sugar, below is equivalent to above
  def combineAll4[A : Monoid](list: List[A]): A = {
    val M = implicitly[Monoid[A]]
    list.foldRight(M.empty)(M.combine)
  }

  // By using `Monoid`'s utility method, above `combineAll4`
  // can be further refactored to the following implementaion
  def combineAll5[A : Monoid](list: List[A]): A = list.foldRight(Monoid[A].empty)(Monoid[A].combine)

  // equivalent to `combineAll5`, when calling `Monoid[A].empty`, it actually call
  // `Monoid.apply[A].empty`
  def combineAll6[A : Monoid](list: List[A]): A =
    list.foldRight(Monoid.apply[A].empty)(Monoid.apply[A].combine)

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y: String): String = x ++ y
  }

  import Demo.{Pair => Paired}
  val res1 = combineAll3(List(Paired(1, "hello"), Paired(2, " "), Paired(3, "world")))
  println(s"combineAll3(List of pairs) = ${res1}")
}

