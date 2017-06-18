package example.cats
package herding.cats
package methodinjection

package ex1 {
  object MethodInjectionApp extends App {
    import ex3.Monoid
    def plus[A: Monoid](a: A, b: A): A = implicitly[Monoid[A]].mappend(a, b)

    import ex6.Monoid._
    println(s"plus(3, 4) = ${plus(3, 4)}")
  }
}

package ex2 {
  import simulacrum._
  @typeclass
  trait Monoid[A] {
    @op("|+|") def mappend(a: A, b: A): A
    def mzero: A
  }

  object Monoid {
    // "ops" gets generated
    val syntax = ops
    implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }
    implicit val StringMonoid: Monoid[String] = new Monoid[String] {
      def mappend(a: String, b: String): String = a + b
      def mzero: String = ""
    }
  }

  object MethodInjectionApp extends App {
    import Monoid._
    import Monoid.syntax._
    println(s"3 |+| 4 = ${3 |+| 4}")
    println(s"a |+| b = ${"a" |+| "b"}")
  }
}
