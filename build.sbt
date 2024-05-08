ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "hs-effects"
  )
