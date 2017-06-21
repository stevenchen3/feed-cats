package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

object FunctorFilter extends App {
  // Scala's for comprehension allows filtering:
  val x = for {
    x <- (1 to 50).toList if x.show contains '7'
  } yield x
  println(s"x=$x")

  val english = Map(1 -> "one", 3 -> "three", 10 -> "ten")
  val y = (1 to 50).toList mapFilter { english.get(_) }
  println(s"y=$y")

  def collectEnglish[F[_]: FunctorFilter](f: F[Int]): F[String] =
    f collect {
      case 1  ⇒ "one"
      case 3  ⇒ "three"
      case 10 ⇒ "ten"
    }
  val z = collectEnglish((1 to 50).toList)
  println(s"z=$z")

  def filterSeven[F[_]: FunctorFilter](f: F[Int]): F[Int] =
    f filter {_.show contains '7'}
  val x1 = filterSeven((1 to 50).toList)
  println(s"x1=$x1")
}
