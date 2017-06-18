package example.cats.herding.cats

package ex1 {
  object SumFunction extends App {
    //def sum(xs: List[Int]): Int = xs.foldLeft(0)((acc, elem) ⇒ acc + elem)
    def sum(xs: List[Int]): Int = xs.foldLeft(0)(_ + _)
    val xs = List(1, 2, 3, 4)
    println(s"sum($xs)=${sum(xs)}")
  }
}

package ex2 {

  /**
    * `Monoid` - It’s a type for which there exists a function `mappend`, which produces another type in
    * the same set; and also a function that produces a zero.
    */
  object IntMonoid {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }
  object SumFunction extends App {
    def sum(xs: List[Int]): Int =
      xs.foldLeft(IntMonoid.mzero)(IntMonoid.mappend)
    val xs = List(1, 2, 3, 4)
    println(s"sum($xs)=${sum(xs)}")
  }
}

package ex3 {
  trait Monoid[A] {
    def mappend(a1: A, a2: A): A
    def mzero: A
  }
  object IntMonoid extends Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }
  object SumFunction extends App {
    def sum(xs: List[Int], m: Monoid[Int]): Int =
      xs.foldLeft(m.mzero)(m.mappend)
    val xs = List(1, 2, 3, 4)
    println(s"sum($xs)=${sum(xs, IntMonoid)}")
  }
}

package ex4 {
  import ex3.{IntMonoid, Monoid}
  object SumFunction extends App {
    def sum[A](xs: List[A], m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)
    val xs = List(1, 2, 3, 4)
    println(s"sum($xs)=${sum(xs, IntMonoid)}")
  }
}

package ex5 {
  import ex3.{IntMonoid, Monoid}
  object DoubleMonoid extends Monoid[Double] {
    def mappend(a: Double, b: Double): Double = a + b
    def mzero: Double = 0.0
  }

  object SumFunction extends App {
    // Make the `monoid` as implicit argument
    def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
      xs.foldLeft(m.mzero)(m.mappend)
    implicit val intMonoid = IntMonoid
    val xs = List(1, 2, 3, 4)
    println(s"sum($xs)=${sum(xs)}")

    implicit val doubleMonoid = DoubleMonoid
    val ys = List(1.0, 2.0, 3.0, 4.0)
    println(s"sum($ys)=${sum(ys)}")
  }
}

package ex6 {
  import ex3.{Monoid}

  // Scala's implicit resolution rules: When it needs an implicit parameter of some type, it’ll
  // look for anything in scope. It’ll include the companion object of the type that you’re
  // looking for.
  object Monoid {
    implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    implicit val DoubleMonoid: Monoid[Double] = new Monoid[Double] {
      def mappend(a: Double, b: Double): Double = a + b
      def mzero: Double = 0
    }

    implicit val StringMonoid: Monoid[String] = new Monoid[String] {
      def mappend(a: String, b: String): String = a + b
      def mzero: String = ""
    }
  }

  object SumFunction extends App {
    // implicit parameter is often written as a context bound
    def sum[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero)(m.mappend)
    }

    // Name these implicit vals whatever you want
    //implicit val intMonoid = IntMonoid
    //implicit val doubleMonoid = DoubleMonoid
    //implicit val stringMonoid: Monoid[String] = new Monoid[String] { // anonymous class
    //  def mappend(a: String, b: String): String = a + b
    //  def mzero: String = ""
    //}

    import Monoid._ // import all implicits
    println(s"sum(List(1, 2, 3, 4))=${sum(List(1, 2, 3, 4))}")
    println(s"sum(List(1.0, 2.0, 3.0, 4.0))=${sum(List(1.0, 2.0, 3.0, 4.0))}")
    println(s"sum(List(a, b, c, d))=${sum(List("a", "b", "c", "d"))}")
  }
}
