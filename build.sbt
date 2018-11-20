name := "JB-internship-problems"

version := "0.1"

scalaVersion := "2.12.7"

val circeVersion = "0.10.0"

libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
