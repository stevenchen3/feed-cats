import sbt._

object Dependencies {
  // typelevel Cats library dependencies, including all modules
  val catsVersion = "0.9.0"
  val catsDeps = Seq(
    "org.typelevel" %% "cats",
    "org.typelevel" %% "cats-laws"
  ).map(_ % catsVersion)

  // Scalatest library dependencies
  val scalatestDeps = Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0" % Test
  )

  // ScalaCheck library dependencies
  val scalaCheckDeps = Seq("org.scalacheck" %% "scalacheck" % "1.13.4" % "test")

  // Specs2 library dependencies
  val specs2Version = "3.8.9"
  val specs2Deps = Seq(
    "org.specs2" %% "specs2-core",
    "org.specs2" %% "specs2-matcher-extra",
    "org.specs2" %% "specs2-cats",
    "org.specs2" %% "specs2-scalaz",
    "org.specs2" %% "specs2-scalacheck",
    "org.specs2" %% "specs2-mock",
    "org.specs2" %% "specs2-analysis",
    "org.specs2" %% "specs2-gwt",
    "org.specs2" %% "specs2-html",
    "org.specs2" %% "specs2-form",
    "org.specs2" %% "specs2-junit"
  ).map(_ % specs2Version).map(_ % "test")

  val resetAllAttrs = Seq("org.scalamacros" %% "resetallattrs" % "1.0.0")

  val simulacrumDeps = Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.10.0"
  )
}
