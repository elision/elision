name := "Elision"

version := "1.0"

// scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.2", "2.10.0")

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-swing" % _ )

addSbtPlugin("com.orrsella" % "sbt-stats" % "1.0.1")

