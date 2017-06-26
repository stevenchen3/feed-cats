package example.typeclass

import org.scalacheck._
import Prop._
import Arbitrary.arbitrary

// Modified based on the following file
// https://github.com/prurph/fsis-scala/blob/master/src/test/scala/ApplicativeTest.scala
abstract class ApplicativeInstanceTest[F[_]](name: String)(implicit
  F: Applicative[F],
  equalFInt: Equal[F[Int]],
  equalFString: Equal[F[String]],
  arbitraryFInt: Arbitrary[F[Int]],
  arbitraryFIntToString: Arbitrary[F[Int ⇒ String]]
  ) extends Properties(s"Applicative [$name]") {

  val laws = ApplicativeLaws[F]

  property("applicative identity") = forAll { (xs: F[Int]) ⇒
    laws.applicativeIdentity(xs).isEqual
  }

  property("applicative homomorphism") = forAll { (a: Int, f: Int ⇒ String) ⇒
    laws.applicativeHomomorphism(a, f).isEqual
  }

  property("applicative interchange") = forAll { (a: Int, ff: F[Int ⇒ String]) ⇒
    laws.applicativeInterchage(a, ff).isEqual
  }

  property("applicative map") = forAll { (fa: F[Int], f: Int ⇒ String) ⇒
    laws.applicativeMap(fa, f).isEqual
  }
}

object ListApplicativeTest extends ApplicativeInstanceTest[List]("List")
object OptionApplicativeTest extends ApplicativeInstanceTest[Option]("Option")
