name := "aoc21"

scalaVersion := "2.13.7"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

Compile / compile / wartremoverErrors ++= Warts.allBut(Wart.StringPlusAny, Wart.Throw, Wart.Nothing)

// scalafix semanticdb
semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.0"
libraryDependencies += "org.typelevel" %% "cats-time" % "0.5.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
