package example.herding.cats

import cats._
import cats.data._
import cats.implicits._
import collection.immutable.BitSet

object DoVsFor extends App {
  def foo =
    for {
      x <- Some(3)
      y <- Some("!")
    } yield x.toString + y
  println(s"foo=$foo") // Some("3!")

  val bits = BitSet(1, 2, 3)
  for {
    x <- bits
  } yield x.toFloat
  println(s"bit=$bits") // List(1.0, 2.0, 3.0)

  val x = for {
    i <- List(1, 2, 3)
    j <- Some(1)
  } yield i + j
  println(s"x=$x") // List(2, 3, 4)

  val y = for {
    i <- Map(1 -> 2)
    j <- Some(3)
  } yield j
  println(s"y = $y") // List(3)

  val z = Monad[Option].flatMap[String, String]({
    val fa0: Option[Int] = 3.some
    Monad[Option].flatMap[Int, String](fa0) { (arg0: Int) ⇒ {
        val next0: Int = arg0
        val x: Int = next0
        val fa1: Option[String] = "!".some
        Monad[Option].flatMap[String, String](fa1)((arg1: String) ⇒ {
          val next1: String = arg1
          val y: String = next1
          Monad[Option].pure[String](x.toString + y)
        })
      }
    }
  }) { (arg2: String) ⇒
    Monad[Option].pure[String](arg2)
  }
  println(s"z=$z")
}
