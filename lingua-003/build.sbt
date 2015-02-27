name := "interpreter"

organization := "toy"

version := "0.1.0"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.3" % "test"

scalacOptions ++= Seq(
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Yno-adapted-args",
  "-Ywarn-value-discard",
  "-language:implicitConversions",
  "-language:higherKinds"
)
