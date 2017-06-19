package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

object ApplyExample extends App {
  // See `Apply` definition
  // https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/Apply.scala
  val x = Apply[Option].ap({ { (_: Int) + 3 }.some })(9.some)
  println(s"Apply[Option].ap({{(_: Int) + 3}.some})(9.some) = ${x}") // expects: Some(12)
  println(s"({(_: Int) + 3}.some) ap 9.some = ${({ (_: Int) + 3 }.some) ap 9.some}")

  // If either side fails, we get None
  val y = Apply[Option].ap({ { (_: String) + "hahah" }.some })(none[String])
  println(s"Apply[Option].ap({{(_: String) + hahah}.some})(none[String]) = ${y}") // expects: None
  val y1 = ({ (_: String) + "hahah" }.some) ap none[String]
  println(s"({(_: String) + hahah}.some) ap none[String] = ${y1}")

  val z = Apply[Option].ap({ none[String ⇒ String] })("woot".some)
  println(s"Apply[Option].ap({none[String ⇒ String]})(woot.some) = ${z}")

  // below is equivalent to
  // Apply[Option].map2(3.some, List(4).some) { _ :: _ }
  val map2Result = (3.some |@| List(4).some) map { _ :: _ }
  println(s"(3.some |@| List(4).some) map { _ :: _ } = ${map2Result}") // expects: Some(List(3, 4))

  val ap2Result = Apply[Option].ap2({ { (_: Int) :: (_: List[Int]) }.some })(3.some, List(4).some)
  println(
    s"Apply[Option].ap2({{(_: Int) :: (_: List[Int])}.some})(3.some, List(4).some) = ${ap2Result}"
  )

  // There’s a special case of map2 called tuple2
  println(s"Apply[Option].tuple2(1.some, 2.some) = ${Apply[Option].tuple2(1.some, 2.some)}") // Some(1, 2)
  println(s"Apply[Option].tuple2(1.some, none[Int]) = ${Apply[Option].tuple2(1.some, none[Int])}")
}
