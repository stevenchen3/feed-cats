package example.typeclass

import org.scalacheck._
import Prop._
import Arbitrary.arbitrary

// Modified based on the following file
// https://github.com/prurph/fsis-scala/blob/master/src/test/scala/MonadTest.scala
abstract class MonadInstanceTest[F[_]](name: String)(implicit
  F: Monad[F],
  equalFInt:  Equal[F[Int]],
  equalFLong: Equal[F[Long]],
  equalFStr:  Equal[F[String]],
  arbFInt:    Arbitrary[F[Int]],
  arbFLong:   Arbitrary[F[Long]],
  arbFString: Arbitrary[F[String]]
  ) extends Properties(s"Monad [$name]") {

  val laws = MonadLaws[F]

  property("monad flatMap associativity") = forAll {
    (fa: F[Int], f: Int ⇒ F[String], g: String ⇒ F[Long]) ⇒
    laws.flatMapAssociativity(fa, f, g).isEqual
  }

  property("monad left identity") = forAll { (a: Int, f: Int ⇒ F[String]) ⇒
    laws.leftIdentify(a, f).isEqual
  }

  property("monad right identity") = forAll { (fa: F[Int]) ⇒
    laws.rightIdentity(fa).isEqual
  }
}

object ListMonadTest extends MonadInstanceTest[List]("List")
object OptionMonadTest extends MonadInstanceTest[Option]("Option")
