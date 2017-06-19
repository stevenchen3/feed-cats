package example.herding.cats
package foldleft

// Typeclasses examples
package ex1 {
  object FoldLeft extends App {
    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) ⇒ B) = xs.foldLeft(b)(f)

    import ex3.{IntMonoid, Monoid}
    def sum[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      foldLeft(xs, m.mzero, m.mappend)
    }

    implicit val intMonoid = IntMonoid
    val xs = List(1, 2, 3, 4)
    println(s"sum($xs)=${sum(xs)}")
  }
}

package ex2 {
  import scala.language.higherKinds
  trait FoldLeft[F[_]] {
    def foldLeft[A, B](xs: F[A], b: B, f: (B, A) ⇒ B): B
  }

  object FoldLeft {
    implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List] {
      def foldLeft[A, B](xs: List[A], b: B, f: (B, A) ⇒ B) = xs.foldLeft(b)(f)
    }
  }

  object FoldLeftApp extends App {
    import example.herding.cats.ex3.Monoid
    // Both Int and List are now pulled out of `sum`
    def sum[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
      val m = implicitly[Monoid[A]]
      val fl = implicitly[FoldLeft[M]]
      fl.foldLeft(xs, m.mzero, m.mappend)
    }

    import example.herding.cats.ex6.Monoid._
    val xs = List(1, 2, 3, 4)
    println(s"sum($xs)=${sum(xs)}")

    val ys = List("a", "b", "c")
    println(s"sum($ys)=${sum(ys)}")
  }
}
