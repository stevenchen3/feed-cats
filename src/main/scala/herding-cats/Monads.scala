package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

object MonadExample extends App {
  type Birds = Int
  case class Pole(left: Birds, right: Birds) {

    def landLeft(n: Birds): Option[Pole] =
      if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none[Pole]

    def landRight(n: Birds): Option[Pole] =
      if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
      else none[Pole]

    def banana: Option[Pole] = none[Pole]
  }

  println(s"Pole(0, 0).landLeft(2) = ${Pole(0, 0).landLeft(2)}")
  println(s"Pole(1, 2).landRight(1) = ${Pole(1, 2).landRight(1)}")
  println(s"Pole(1, 2).landRight(-1) = ${Pole(1, 2).landRight(-1)}")

  // chain the `landLeft`/`landRight` using `flatMap` or its symbolic alias `>>=`
  val rlr = Monad[Option].pure(Pole(0, 0)) >>= { _.landRight(2) } >>= { _.landLeft(2) } >>= {
    _.landRight(2)
  }
  println(s"rlr=${rlr}")

  // monadic chaining simulates the pole balancing better
  // x >>= y is equivalent to x.>>=(y)
  // In this chain of monadic functions, the effect from one function is carried over to the next.
  val lrlr = Monad[Option]
    .pure(Pole(0, 0))
    .>>= { _.landLeft(1) }
    .>>= { _.landRight(4) }
    .>>= { _.landLeft(-1) }
    .>>= { _.landRight(-2) }
  println(s"lrlr=$lrlr")

  val lbl = Monad[Option].pure(Pole(0, 0)) >>= { _.landLeft(1) } >>= { _.banana } >>= {
    _.landRight(1)
  }
  println(s"lbl=$lbl")

  // Instead of making functions that ignore their input and just return a predetermined monadic
  // value, we can use the `>>` function.
  val x = none[Int] >> 3.some
  println(s"none[Int] >> 3.some = ${x}")
  println(s"3.some >> 4.some = ${3.some >> 4.some}")
  println(s"3.some >> 4.some >> 5.some = ${3.some >> 4.some >> 5.some}")
  println(s"3.some >> none[Int] = ${3.some >> none[Int]}")

  val lblx = (Monad[Option].pure(Pole(0, 0)) >>= { _.landLeft(1) }) >> none[Pole] >>= {
    _.landRight(1)
  }
  println(s"lblx=$lblx")

  val y = 3.some >>= { x ⇒
    "!".some >>= { y ⇒
      (x.show + y).some
    }
  } // Some("3!")
  println(s"y = $y")
  val z = 3.some >>= { x ⇒
    none[String] >>= { y ⇒
      (x.show + y).some
    }
  } // Option[String]: None
  println(s"z=$z")

  val y1 = (none: Option[Int]) >>= { x ⇒
    "!".some >>= { y ⇒
      (x.show + y).some
    }
  } // Option[String]: None
  println(s"y1=$y1")
  val z1 = 3.some >>= { x ⇒
    "!".some >>= { y ⇒
      none[String]
    }
  } // Option[String]: None
  println(s"z1=$z1")

  val y2 = for {
    x <- 3.some
    y <- "!".some
  } yield (x.show + y)
  println(s"y2=$y2")

  // Our tightwalker's routine can also be expressed with do notation.
  def routine: Option[Pole] =
    for {
      start <- Monad[Option].pure(Pole(0, 0))
      first <- start.landLeft(2)
      second <- first.landRight(2)
      third <- second.landLeft(1)
    } yield third
  println(s"routine=$routine")

  // If we want to throw the Pierre a banana peel in do notation, we can do the following:
  def routine1: Option[Pole] =
    for {
      start <- Monad[Option].pure(Pole(0, 0))
      first <- start.landLeft(2)
      _ <- none[Pole]
      second <- first.landRight(2)
      third <- second.landLeft(1)
    } yield third
  println(s"routine1=$routine1")

  // In do notation, when we bind monadic values to names, we can utilize pattern matching, just like
  // in let expressions and function parameters.
  def justH: Option[Char] =
    for {
      (x :: xs) <- "hello".toList.some
    } yield x
  println(s"justH=${justH}")

  // When pattern matching fails in a do expression, the fail function is called. It’s part of the
  // Monad type class and it enables failed pattern matching to result in a failure in the context
  // of the current monad instead of making our program crash.

  def wopwop: Option[Char] =
    for {
      (x :: xs) <- "".toList.some
    } yield x
  println(s"wopwop=$wopwop")

  // Monad had three laws:
  //
  // - (1) left identity (unit): (Monad[F].pure(x) flatMap {f}) === f(x)
  //   in other word, unit(x) flatMap f === f(x)
  def f(x: Int): List[Int] = List(x + 1)
  assert(List(1).flatMap(x ⇒ f(x)) === f(1))
  // The first monad law states that if we take a value, put it in a default context with return and
  // then feed it to a function by using >>=, it's the same as just taking the value and applying
  // the function to it.
  assert {
    (Monad[Option].pure(3) >>= { x ⇒
      (x + 100000).some
    }) === ({ (x: Int) ⇒
      (x + 100000).some
    })(3)
  }

  // - (2) right identity (unit): (m flatMap {Monad[F].pure(_)}) === m
  //   in other word, m flatMap unit = m, e.g., List(1).flatMap(x ⇒ List(x))
  assert(List(1).flatMap(x ⇒ List(x)) === List(1))
  assert { ("move on up".some >>= { Monad[Option].pure(_) }) === "move on up".some }

  // - (3) associativity: (m flatMap f) flatMap g === m flatMap { x ⇒ f(x) flatMap {g} }
  def g(x: Int): List[Int] = List(x + 2)
  assert(List(1).flatMap(f).flatMap(g) == List(1).flatMap { x ⇒
    f(x) flatMap g
  })
  // The final monad law says that when we have a chain of monadic function applications with >>=,
  // it shouldn’t matter how they’re nested.
  val leftVal = Monad[Option].pure(Pole(0, 0)) >>= { _.landRight(2) } >>= { _.landLeft(2) } >>= {
    _.landRight(2)
  }
  val rightVal = Monad[Option].pure(Pole(0, 0)) >>= { x =>
    x.landRight(2) >>= { y =>
      y.landLeft(2) >>= { z =>
        z.landRight(2)
      }
    }
  }
  assert(leftVal == rightVal)
}
