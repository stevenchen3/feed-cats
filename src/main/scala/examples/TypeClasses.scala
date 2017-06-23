package example.cats

/*
 *
 * Type classes are a powerful tool used in functional programming to enable ad-hoc polymorphism,
 * more commonly known as overloading. Where many object-oriented languages leverage subtyping for
 * polymorphic code, functional programming tends towards a combination of parametric polymorphism
 * (think type parameters, like Java generics) and ad-hoc polymorphism.
 *
 */
object TypeClasses extends App {
  // collapse a list, sum a list of integers, concate a list of strings, and union a list of sets
  def sumInts(list: List[Int]): Int = list.foldRight(0)(_ + _)

  def concatStrings(list: List[String]): String = list.foldRight("")(_ ++ _)

  def unionSets[A](list: List[Set[A]]): Set[A] = list.foldRight(Set.empty[A])(_ union _)

  // something in common among above three: all take an initial value and a combining function

  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
  }

  val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }

  def combineAll[A](list: List[A], a: Monoid[A]): A = list.foldRight(a.empty)(a.combine)
  val xs = List(1, 2, 3, 4)
  assert(sumInts(xs) == combineAll(xs, intAdditionMonoid))
}
