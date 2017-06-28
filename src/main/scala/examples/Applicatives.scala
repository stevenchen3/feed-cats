package example.cats.applicative

object ApplicativeApp extends App {
  import cats.Applicative
  // defines implicit value for evidence parameter of type cats.Applicative[List]
  import cats.instances.list._

  def product3[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] = {
    val F: Applicative[F] = Applicative[F]
    val abc = F.product(F.product(fa, fb), fc)
    F.map(abc) {
      case ((a, b), c) ⇒ (a, b, c)
    }
  }

  val fa = List(1, 2)
  val fb = List(3, 4)
  val fc = List(5, 6)
  println(s"product3(fa, fb, fc) = ${product3(fa, fb, fc)}")

  import cats.instances.option._
  import cats.syntax.traverse._
  val res1 = List(1, 2, 3).traverse(x ⇒ Some(x): Option[Int])
  println(s"List(1, 2, 3).traverse(x ⇒ Some(x): Option[Int]) = $res1")

  //import cats.implicits._
  //val o1: Option[Int] = Some(42)
  //val o2: Option[String] = Some("hello")
  //val res2 = (o1 |@| o2).map((i: Int, s: String) ⇒ i.toString ++ s)
  //println(s"res2 = $res2")

  //val res3 = (o1 |@| o2).tupled
  //println(s"(o1 |@| o2).tupled = ${res3}")
}
