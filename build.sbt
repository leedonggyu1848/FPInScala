ThisBuild / scalaVersion := "3.3.0"
ThisBuild / organization := "com.example"

lazy val fpInScala = (project in file("."))
  .settings(
    name := "FpInScala",
    libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.16" % "test"
  )
