package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

object ListDataType extends App {
  val x = (List(1, 2, 3) |@| List(10, 100, 100)) map { _ * _ }
  println(s"(List(1, 2, 3) |@| List(10, 100, 100)) map {_ * _} = $x")

  val y = List(3, 4, 5) >>= { x ⇒
    List(x, -x)
  }
  println(s"List(3, 4, 5) >>= {x ⇒ List(x, -x)} = $y")

  // So in this monadic view, a List context represents a mathematical value that could have
  // multiple solutions. Other than that manipulating Lists using for notation is just like plain
  // Scala:
  for {
    n <- List(1, 2)
    ch <- List('a', 'b')
  } yield (n, ch)
}
