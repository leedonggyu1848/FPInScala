ThisBuild / scalaVersion := "3.3.5"
ThisBuild / organization := "com.example"

lazy val fpInScala = (project in file("."))
  .settings(
    name := "FpInScala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
)
