resolvers += Resolver.typesafeRepo("releases")

libraryDependencies += "com.geirsson" %% "scalafmt-bootstrap" % "0.6.6"

// Add the sbt-doctest plugin
addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.5.0")
// Add the code coverage plugin
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")
// Add the scala style check plugin
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")
// Add dependency tree graph plugin
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
// Add `tut` documentation plugin
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.5.2")
