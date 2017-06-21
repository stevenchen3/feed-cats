package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

object MonadFilterExample extends App {
  // We can use this to get an empty value of a datatype that supports it
  println(s"MonadFilter[List].empty[Int] = ${MonadFilter[List].empty[Int]}")

  case class KnightPos(c: Int, r: Int) {
    def move: List[KnightPos] =
      for {
        KnightPos(c2, r2) <- List(KnightPos(c + 2, r - 1), KnightPos(c + 2, r + 1),
          KnightPos(c - 2, r - 1), KnightPos(c - 2, r + 1),
          KnightPos(c + 1, r - 2), KnightPos(c + 1, r + 2),
          KnightPos(c - 1, r - 2), KnightPos(c - 1, r + 2)) if (
            ((1 to 8).toList contains c2) && ((1 to 8).toList contains r2))
      } yield KnightPos(c2, r2)

    def in3: List[KnightPos] =
      for {
        first <- move
        second <- first.move
        third <- second.move
      } yield third

    def canReachIn3(end: KnightPos): Boolean = in3 contains end
  }
  println(s"KnightPos(6, 2) = ${KnightPos(6, 2).move}")
  println(s"KnightPos(8, 1).move = ${KnightPos(8, 1).move}")

  val x = KnightPos(6, 2) canReachIn3 KnightPos(6, 1)
  println(s"KnightPos(6, 2) canReachIn3 KnightPos(6, 1) = $x")
  val y = KnightPos(6, 2) canReachIn3 KnightPos(7, 3)
  println(s"KnightPos(6, 2) canReachIn3 KnightPos(7, 3) = $y")
}
