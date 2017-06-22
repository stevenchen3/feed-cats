package example.herding.cats

import annotation.tailrec

import cats._
import cats.data._
import cats.implicits._

// See http://eed3si9n.com/herding-cats/Writer.html
object PerformanceComparison extends App {
  def vectorFinalCountDown(x: Int): Writer[Vector[String], Unit] = {
    @tailrec
    def doFinalCountDown(x: Int, w: Writer[Vector[String], Unit]): Writer[Vector[String], Unit] =
      x match {
        case y if y <= 0 ⇒
          w >>= { _ ⇒
            Writer.tell(Vector("0"))
          }
        case x ⇒
          doFinalCountDown(x - 1, w >>= { _ ⇒
            Writer.tell(Vector(x.show))
          })
      }
    val t0 = System.currentTimeMillis
    val r = doFinalCountDown(x, Writer.tell(Vector[String]()))
    val t1 = System.currentTimeMillis
    r >>= { _ ⇒
      Writer.tell(Vector((t1 - t0).show + " msec"))
    }
  }

  def listFinalCountDown(x: Int): Writer[List[String], Unit] = {
    @tailrec
    def doFinalCountDown(x: Int, w: Writer[List[String], Unit]): Writer[List[String], Unit] =
      x match {
        case y if y <= 0 ⇒
          w >>= { _ ⇒
            Writer.tell(List("0"))
          }
        case x ⇒
          doFinalCountDown(x - 1, w >>= { _ ⇒
            Writer.tell(List(x.show))
          })
      }
    val t0 = System.currentTimeMillis
    val r = doFinalCountDown(x, Writer.tell(List[String]()))
    val t1 = System.currentTimeMillis
    r >>= { _ ⇒
      Writer.tell(List((t1 - t0).show + " msec"))
    }
  }

  val vectorPerf = vectorFinalCountDown(10000).run._1.last
  val listPerf = listFinalCountDown(10000).run._1.last
  println(s"vectorPerf=${vectorPerf}")
  println(s"listPerf=${listPerf}")
}
