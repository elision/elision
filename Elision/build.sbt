name := "Elision"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-swing" % _ )

addSbtPlugin("com.orrsella" % "sbt-stats" % "1.0.1")

