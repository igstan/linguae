name := "debugger"

organization := "ro.igstan"

version := "0.1.0"

scalaVersion := "2.11.6"

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
  "org.scala-js"  %%% "scalajs-dom" % "0.8.0",
  "org.scalatest"  %% "scalatest"   % "2.2.4" % "test"
)

enablePlugins(ScalaJSPlugin)

graphSettings

workbenchSettings

bootSnippet := "ro.igstan.debugger.Main().main();"

updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)
