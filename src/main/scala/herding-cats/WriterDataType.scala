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
  //
  // `Writer` actually wraps a tuple of a monad and a value
  // `Writer` is defined as follows
  //
  // type Writer[L, V] = WriterT[Id, L, V]
  // object Writer {
  //   def apply[L, V](l: L, v: V): WriterT[Id, L, V] = WriterT[Id, L, V]((l, v))
  //   def value[L:Monoid, V](v: V): Writer[L, V] = WriterT.value(v)
  //   def tell[L](l: L): Writer[L, Unit] = WriterT.tell(l)
  // }
  //
  // See https://github.com/typelevel/cats/blob/v0.7.2/core/src/main/scala/cats/data/package.scala
  val w = Writer("Smallish gang.", 3)
  val v = Writer.value[String, Int](3)
  val l = Writer.tell[String]("Log something")
  // `writer.run` returns the tuple value
  println(s"writer=$w\nwriter.value=$v\nwriter.tell=$l\nwriter.run=${w.run}")

  def logNumber(x: Int): Writer[List[String], Int] = Writer(List("Got number: " + x.show), 3)
  def multWithLog: Writer[List[String], Int] =
    for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b
  println(s"multWithLog=$multWithLog")
  println(s"multWithLog.run=${multWithLog.run}") // (List(Got number: 3, Got number: 5),9)

  // the above expression is equivalent to below expression
  def multWithLog2: Writer[List[String], Int] = {
    logNumber(3).flatMap(a ⇒ for (b <- logNumber(5)) yield a * b)
  }
  println(s"multWithLog2=$multWithLog2")
  println(s"multWithLog2.run=${multWithLog2.run}") // (List(Got number: 3, Got number: 5),9)

  /*
   * `flatMap` on `WriterT` is defined as follows:
   *
   * def flatMap[U](f: V => WriterT[F, L, U])(implicit flatMapF: FlatMap[F], semigroupL: Semigroup[L]): WriterT[F, L, U] =
   *  WriterT {
   *    flatMapF.flatMap(run) { lv =>
   *      flatMapF.map(f(lv._2).run) { lv2 =>
   *        (semigroupL.combine(lv._1, lv2._1), lv2._2)
   *      }
   *    }
   * }
   */

  // Add logging to program
  def gcd(a: Int, b: Int): Writer[List[String], Int] = b match {
    case 0 ⇒
      for {
        _ <- Writer.tell(List("Finished with " + a.show))
      } yield a
    case _ ⇒
      // `>>=` is an alias for `flatMap`
      Writer.tell(List(s"${a.show} mod ${b.show} = ${(a % b).show}")) >>= { _ ⇒ gcd(b, a % b) }
  }
  println(s"gcd(10, 15)=${gcd(10, 15)}")
  println(s"gcd(10, 15)=${gcd(10, 15).run}")
  println(s"gcd(10, 15)=${gcd(10, 15).value}")
}
