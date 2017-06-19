import Dependencies._

lazy val commonSettings = Seq(
  name := "feed-cats",
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
    "-language:implicitConversions"
  ),
  javacOptions in Compile ++= Seq(
    "-source",
    "1.8",
    "-target",
    "1.8",
    "-Xlint:unchecked",
    "-Xlint:deprecation"
  ),
  javaOptions in Test ++= Seq("-Xms1024m", "-Xmx2048m", "-Dconfig.resource=test.conf"),
  javaOptions in run ++= Seq("-Xms1024m", "-Xmx2048m", "-XX:+UseParallelGC", "-server")
)

lazy val root = Project(id = "feed-cats", base = file("."))
  .settings(commonSettings: _*)
  .settings(fork in run := false)
  .settings(fork in Test := true)
  .settings(coverageEnabled := true) // change to `false` when comes to packaging and distribution
  .settings(doctestWithDependencies := false)
  .settings(addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch))
  .settings(libraryDependencies ++= scalatestDeps)
  .settings(libraryDependencies ++= scalaCheckDeps)
  .settings(libraryDependencies ++= catsDeps)
  .settings(libraryDependencies ++= specs2Deps)

lazy val latestScalafmt = "1.0.0-RC1"
commands += Command.args("scalafmt", "Run scalafmt cli.") {
  case (state, args) ⇒
    val Right(scalafmt) =
      org.scalafmt.bootstrap.ScalafmtBootstrap.fromVersion(latestScalafmt)
    scalafmt.main("--non-interactive" +: args.toArray)
    state
}
