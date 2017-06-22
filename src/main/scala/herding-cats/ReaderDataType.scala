package example.herding.cats

import cats._
import cats.data._
import cats.implicits._

// See http://eed3si9n.com/herding-cats/Reader.html
object ReaderDataType extends App {
  val f  = (_: Int) * 2 // Int ⇒ Int
  val g  = (_: Int) + 10 // Int ⇒ Int
  val gf = g map f // Int ⇒ Int, equivalent to f(g(x))
  println(s"(g map f)(8) = ${(g map f)(8)}")

  // Not only is the function type (->) r a functor and an applicative functor, but it’s also a
  // monad. Just like other monadic values that we’ve met so far, a function can also be considered
  // a value with a context. The context for functions is that that value is not present yet and
  // that we have to apply that function to something in order to get its result value.
  val h = (f |@| g) map { _ + _ }
  println(s"h(3) = ${h(3)}") // expect 19

  // above is equivalent to below `addStuff`
  //
  // Both (*2) and (+10) get applied to the number 3 in this case. return (a+b) does as well, but it
  // ignores it and always presents a+b as the result. For this reason, the function monad is also
  // called the reader monad. All the functions read from a common source.
  val addStuff: Int ⇒ Int = for {
    a <- (_: Int) * 2
    b <- (_: Int) + 10
  } yield a + b

  // dependency injection
  case class User(id: Long, parentId: Long, name: String, email: String)
  trait UserRepo {
    def get(id: Long): User
    def find(name: String): User
  }


  trait Users {
    def getUser(id: Long): UserRepo ⇒ User = {
      case repo ⇒ repo.get(id)
    }
    def findUser(name: String): UserRepo ⇒ User = {
      case repo ⇒ repo.find(name)
    }
  }

  object UserInfo extends Users {
    def userInfo(name: String): UserRepo ⇒ Map[String, String] =
      for {
        user <- findUser(name)
        boss <- getUser(user.parentId)
      } yield Map(
        "name" -> s"${user.name}",
        "email" -> s"${user.email}",
        "boss_name" -> s"${boss.name}"
      )
  }

  trait Program {
    def app: UserRepo ⇒ String =
      for {
        fredo <- UserInfo.userInfo("Fredo")
      } yield fredo.toString
  }

  val testUsers = List(
    User(0, 0, "Vito", "vito@example.com"),
    User(1, 0, "Michael", "michael@example.com"),
    User(2, 0, "Fredo", "fredo@example.com")
  )

  def mkUserRepo: UserRepo = new UserRepo {
    def get(id: Long): User = (testUsers find { _.id === id }).get
    def find(name: String): User = (testUsers find { _.name === name }).get
  }

  println(s"result = ${mkUserRepo}")
}
