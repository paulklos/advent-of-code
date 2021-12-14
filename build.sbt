ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "Advent of code 2021"
  )

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test->default"

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
