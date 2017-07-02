package example.cats.foldable

import cats._
import cats.implicits._

object FoldableApp extends App {
  val res1 = Foldable[List].fold(List(1, 2, 3, 4))
  println(s"Foldable[List].fold(List(1, 2, 3, 4)) = ${res1}")

  val res2 = Foldable[List].fold(List("a", "b", "c"))
  println(s"""Foldable[List].fold(List("a", "b", "c")) = ${res2}""")

  val res3= Foldable[List].foldMap(List(1, 2, 4))(_.toString)
  println(s"""Foldable[List].foldMap(List(1, 2, 4))(_.toString) = ${res3}""")

  val res4 = Foldable[List].foldMap(List(1, 2, 4))(x ⇒ x + 1)
  println(s"""Foldable[List].foldMap(List(1, 2, 4))(x ⇒ x + 1) = $res4""")

  val res5 = Foldable[List].foldK(List(List(1,2,3), List(4,5,6)))
  println(s"""Foldable[List].foldK(List(List(1,2,3), List(4,5,6))) = $res5""")

  val res6 = Foldable[List].reduceLeftToOption(List(1,2,3,4))(_.toString)((s,i) ⇒ s + i)
  println(s"reduceLeftToOption = $res6")

  val res7 = Foldable[List].reduceRightToOption(List(1,2,3,4))(_.toString)((i,s) ⇒ Later(s.value + i)).value
  println(s"reduceRightToOption = $res7")

  def parseInt(s: String): Option[Int] = scala.util.Try(Integer.parseInt(s)).toOption

  val res8 = Foldable[List].traverse_(List("1", "2"))(parseInt)
  println(s"""Foldable[List].traverse_(List("1", "2"))(parseInt) = $res8""")

  val res9 = Foldable[List].traverse_(List("1", "A"))(parseInt)
  println(s"""Foldable[List].traverse_(List("1", "A"))(parseInt) = $res9""")

  val res10 = Foldable[List].sequence_(List(Option(1), Option(2)))
  println(s"""Foldable[List].sequence_(List(Option(1), Option(2))) = $res10""")

  val res11 = Foldable[List].sequence_(List(Option(1), None))
  println(s"""Foldable[List].sequence_(List(Option(1), None))=$res11""")

  import cats.data.Nested
  val listOption0 = Nested(List(Option(1), Option(2), Option(3)))
  val listOption1 = Nested(List(Option(1), Option(2), None, Option(4)))
  val res12 = Foldable[Nested[List, Option, ?]].fold(listOption0)
  val res13 = Foldable[Nested[List, Option, ?]].fold(listOption1)
  println(s"""Foldable[Nested[List, Option, ?]].fold(listOption0) = $res12""") // 6
  println(s"""Foldable[Nested[List, Option, ?]].fold(listOption1) = $res13""") // 7

  val res14 = Foldable[List].dropWhile_(List[Int](2,4,5,6,7))(_ % 2 == 0)
  println(s"""Foldable[List].dropWhile_(List[Int](2,4,5,6,7))(_ % 2 == 0) = $res14""")

  val res15 = Foldable[List].dropWhile_(List[Int](1,2,4,5,6,7))(_ % 2 == 0)
  println(s"""Foldable[List].dropWhile_(List[Int](1,2,4,5,6,7))(_ % 2 == 0) = $res15""")

  /**
    * Note that, in order to support laziness, the signature of Foldable’s foldRight is
    *
    * def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B]
    *
    * as opposed to
    *
    * def foldRight[A, B](fa: F[A], z: B)(f: (A, B) ⇒ B): B
    *
    * This will prevent operations which are lazy in their right hand argument to traverse the
    * entire structure unnecessarily.
    *
    */
  val allFalse = Stream.continually(false)
  try {
    allFalse.foldRight(true)(_ && _)
  } catch {
    case e:StackOverflowError ⇒ println(e)
  }

  val res16 = Foldable[Stream].foldRight(allFalse, Eval.True)((a,b) ⇒ if (a) b else Eval.now(false)).value
  println(s"""$res16""")
}
