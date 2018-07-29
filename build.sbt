import Dependencies._

lazy val commonSettings = Seq(
  name := "ragdoll",
  version := "0.9.0",
  scalaVersion := "2.11.8",
  scalacOptions in Compile ++= Seq(
    "-encoding",
    "UTF-8",
    "-target:jvm-1.8",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlog-reflective-calls",
    "-Xlint",
    "-Yrangepos",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros"
  ),
  javacOptions in Compile ++= Seq(
    "-source",
    "1.8",
    "-target",
    "1.8",
    "-Xlint:unchecked",
    "-Xlint:deprecation"
  ),
  javaOptions in Test ++= Seq("-Xms256m", "-Xmx2g", "-Dconfig.resource=test.conf"),
  javaOptions in run  ++= Seq("-Xms256m", "-Xmx2g", "-XX:+UseParallelGC", "-server")
)

lazy val tutSettings = Seq(
  tutSourceDirectory := (baseDirectory( _ / "docs/src/main/tut"  )).value
)

resolvers += Resolver.sonatypeRepo("releases")
val macroParaside = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
val kindProjector = compilerPlugin("org.spire-math" % "kind-projector" % "0.9.4" cross CrossVersion.binary)

lazy val root = Project(id = "ragdoll", base = file("."))
  .enablePlugins(TutPlugin)
  .settings(commonSettings: _*)
  .settings(tutSettings)
  .settings(fork in run := true)
  .settings(fork in Test := true)
  .settings(coverageEnabled := true) // change to `false` when comes to packaging and distribution
  .settings(doctestWithDependencies := false)
  .settings(libraryDependencies ++= scalatestDeps)
  .settings(libraryDependencies ++= scalaCheckDeps)
  .settings(libraryDependencies ++= catsDeps)
  .settings(libraryDependencies ++= specs2Deps)
  .settings(libraryDependencies ++= Seq(macroParaside, kindProjector))
  .settings(libraryDependencies ++= resetAllAttrs)
  .settings(libraryDependencies ++= simulacrumDeps)

lazy val latestScalafmt = "1.0.0-RC1"
commands += Command.args("scalafmt", "Run scalafmt cli.") {
  case (state, args) ⇒
    val Right(scalafmt) =
      org.scalafmt.bootstrap.ScalafmtBootstrap.fromVersion(latestScalafmt)
    scalafmt.main("--non-interactive" +: args.toArray)
    state
}
