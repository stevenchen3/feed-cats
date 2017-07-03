package example.cats.invariant

import java.util.Date

import cats._
import cats.implicits._

/**
  *
  * Reusing the example of turning Semigroup[Long] into Semigroup[Date], we can use the g parameter
  * to turn Date into a Long, combine our two values using Semigroup[Long] and then convert the
  * result back into a Date using the f parameter of imap
  */
object InvariantApp extends App {
  def longToDate: Long ⇒ Date = new Date(_)
  def dateToLong: Date ⇒ Long = _.getTime

  //implicit val semigroupDate: Semigroup[Long] = Semigroup[Long].imap(longToDate)(dateToLong)
  //val somedate: Date = longToDate(1449088684104l)
  //val timeLeft: Date = longToDate(1900918893l)

  //val combineDate: Date = somedate |+| timeLeft

  //println(s"""combineDate = ${combineDate}""")
}
