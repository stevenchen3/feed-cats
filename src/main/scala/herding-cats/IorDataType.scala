package example.herding.cats

import cats._
import cats.data._
import cats.data.{NonEmptyList ⇒ NEL}
import cats.implicits._

object IorDataType extends App {
  println(s"Ior.right[NEL[String], Int](1) = ${Ior.right[NEL[String], Int](1)}")
  val left = Ior.left[NEL[String], Int](NEL.of("error"))
  println(s"Ior.left[NEL[String], Int](NEL.of(error)) = $left")
  val both = Ior.both[NEL[String], Int](NEL.of("warning"), 1)
  println(s"Ior.both[NEL[String], Int](NEL.of(warning), 1) = $both")

  // As noted in the scaladoc comment, Ior’s flatMap uses Semigroup[A] to accumulate failures when
  // it sees an Ior.both(...) value. So we could probably use this as a hybrid of Xor and Validated.
  val com1 = Ior.right[NEL[String], Int](1) >>= { x ⇒
    Ior.right[NEL[String], Int](x + 1)
  }
  println(s"Ior.right[NEL[String], Int](1) >>= { x ⇒ Ior.right[NEL[String], Int](x + 1)} = $com1")

  val com2 = Ior.left[NEL[String], Int](NEL.of("error 1")) >>= { x ⇒
    Ior.right[NEL[String], Int](x + 1)
  }
  println(s"com2=$com2")

  val com3 = Ior.both[NEL[String], Int](NEL.of("warning 1"), 1) >>= { x ⇒
    Ior.right[NEL[String], Int](x + 1)
  }
  println(s"com3=$com3")

  val com4 = Ior.right[NEL[String], Int](1) >>= { x ⇒
    Ior.left[NEL[String], Int](NEL.of("error 2"))
  }
  println(s"com4=$com4")

  val com5 = Ior.left[NEL[String], Int](NEL.of("error 1")) >>= { x ⇒
    Ior.left[NEL[String], Int](NEL.of("error 2"))
  }
  println(s"com5=$com5")

  val com6 = Ior.both[NEL[String], Int](NEL.of("warning 1"), 1) >>= { x ⇒
    Ior.left[NEL[String], Int](NEL.of("error 2"))
  }
  println(s"com6=$com6")

  val com7 = Ior.right[NEL[String], Int](1) >>= { x ⇒
    Ior.both[NEL[String], Int](NEL.of("warning 2"), x + 1)
  }
  println(s"com7=$com7")

  val com8 = Ior.left[NEL[String], Int](NEL.of("error 1")) >>= { x ⇒
    Ior.both[NEL[String], Int](NEL.of("warning 2"), x + 1)
  }
  println(s"com8=$com8")

  val com9 = Ior.both[NEL[String], Int](NEL.of("warning 1"), 1) >>= { x ⇒
    Ior.both[NEL[String], Int](NEL.of("warning 2"), x + 1)
  }
  println(s"com9=$com9")

  // Ior.left short curcuits like the failure values in Xor[A, B] and Either[A, B], but Ior.both
  // accumulates the failure values like Validated[A, B]
  val res = for {
    e1 <- Ior.right[NEL[String], Int](1)
    e2 <- Ior.both[NEL[String], Int](NEL.of("event 2 warning"), e1 + 1)
    e3 <- Ior.both[NEL[String], Int](NEL.of("event 3 warning"), e2 + 1)
  } yield (e1 |+| e2 |+| e3)
  println(s"res = $res")
}
