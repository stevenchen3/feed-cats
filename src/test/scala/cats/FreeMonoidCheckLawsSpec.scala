package example.herding.cats

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import FreeMonoids._

object FreeMonoidCheckLawsSpec extends Properties("FreeMonoids") {
  property("free monoid laws") = forAll { (c: Char) ⇒
    f(c) == g(c)
  }
}
