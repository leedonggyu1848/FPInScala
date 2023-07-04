ThisBuild / scalaVersion := "2.13.8"
ThisBuild / organization := "com.example"

lazy val fpInScala = (project in file("."))
  .settings(
    name := "FpInScala",
    libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.16" % "test"
  )
