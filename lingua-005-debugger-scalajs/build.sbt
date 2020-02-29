name := "debugger"

organization := "ro.igstan"

version := "0.1.0"

scalaVersion := "2.12.10"

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

scalacOptions ++= Seq(
  "-feature",
  "-unchecked",
  "-deprecation",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xlint:_",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-value-discard"
)

libraryDependencies ++= Seq(
  "org.scala-js"  %%% "scalajs-dom" % "1.0.0",
  "org.scalatest"  %% "scalatest"   % "3.1.1" % "test"
)

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true
