package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

object StateDataType extends App {
  type Stack = List[Int]
  def pop(s0: Stack): (Stack, Int) =
    s0 match {
      case x :: xs ⇒ (xs, x)
      case Nil     ⇒ sys.error("stack is empty")
    }

  def push(s0: Stack, a: Int): (Stack, Unit) = (a :: s0, ())

  def stackManip(s0: Stack): (Stack, Int) = {
    val (s1, _) = push(s0, 3)
    val (s2, a) = pop(s1)
    pop(s2)
  }

  println(s"stackManip(List(5, 8, 2, 1)) = ${stackManip(List(5, 8, 2, 1))}")

  // Implement above using `State` in `cats`
  val pop2: State[Stack, Int] = State[Stack, Int] {
    case x :: xs ⇒ (xs, x)
    case Nil     ⇒ sys.error("stack is empty")
  }

  def push2(a: Int): State[Stack, Unit] = State[Stack, Unit] {
    case xs ⇒ (a :: xs, ())
  }

  def stackManip2: State[Stack, Int] = for {
    _ <- push2(3)
    a <- pop2
    b <- pop2
  } yield(b)

  println(s"stackManip2.run(List(5, 8, 2, 1)).value = ${stackManip2.run(List(5, 8, 2, 1)).value}")

  // Use `State` helper functions
  def stackyStack: State[Stack, Unit] = for {
    stackNow <- State.get[Stack]
    r <- if (stackNow === List(1, 2, 3)) State.set[Stack](List(8, 3, 1))
    else State.set[Stack](List(9, 2, 1))
  } yield r
  val x = stackyStack.run(List(1, 2, 3)).value
  println(s"stackyStack.run(List(1, 2, 3)).value = $x")

  // Implement `pop` and `push` using `get` and `set`
  val pop3: State[Stack, Int] = for {
    s <- State.get[Stack]
    (x :: xs) = s
    _ <- State.set[Stack](xs)
  } yield x

  def push3(x: Int): State[Stack, Unit] = for {
    xs <- State.get[Stack]
    r <- State.set(x :: xs)
  } yield r
}
