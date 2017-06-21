package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

object WriterDataType extends App {
  def isBigGang(x: Int): (Boolean, String) = (x > 9, "Compare gang size to 9")

  // Since method injection is a common use case for implicits, Scala 2.10 adds a syntax sugar
  // called implicit class to make the promotion from a class to an enriched class easier.
  implicit class PairOps[A](pair: (A, String)) {
    def applyLog[B](f: A ⇒ (B, String)): (B, String) = {
      val (x, log) = pair // x: A,    log: String
      val (y, newlog) = f(x) // y: B, newlog: String
      (y, log ++ newlog)
    }
  }
  val x = (3, "Smallish gang.") applyLog isBigGang
  println(s"(3, Smallish gang.) = ${x}")

  // Use `Semigroup`
  implicit class PairOps2[A, B: Semigroup](pair: (A, B)) {
    def applyLog2[C](f: A ⇒ (C, B)): (C, B) = {
      val (x, log) = pair
      val (y, newlog) = f(x)
      (y, log |+| newlog)
    }
  }
  val y = (13, "Bigger gang.") applyLog isBigGang
  println(s"(13, Bigger gang.) = ${y}")

  // Cat's `Writer`
  val w = Writer("Smallish gang.", 3)
  val v = Writer.value[String, Int](3)
  val l = Writer.tell[String]("Log something")
  println(s"w=$w, v=$v, l=$l, w.run=${w.run}")

  def logNumber(x: Int): Writer[List[String], Int] = Writer(List("Got number: " + x.show), 3)
  def multWithLog: Writer[List[String], Int] =
    for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b
  println(s"multWithLog.run=${multWithLog.run}") // (List(Got number: 3, Got number: 5),9)

}
