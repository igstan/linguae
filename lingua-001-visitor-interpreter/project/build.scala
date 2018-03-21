import sbt._, Keys._

import net.virtualvoid.sbt.graph.Plugin.graphSettings

object Build extends Build {
  val root = Project(
    id = "lingua-001",
    base = file("."),
    settings = graphSettings ++ Seq(
      name := "lingua-001",
      organization := "ro.igstan",
      version := "0.1.0",
      scalaVersion := "2.11.1",
      crossPaths := false,
      // http://docs.oracle.com/javase/7/docs/technotes/tools/solaris/javac.html#xlintwarnings
      javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),
      scalacOptions ++= Seq(
        "-feature",
        "-unchecked",
        "-deprecation",
        "-Yno-adapted-args",
        "-language:implicitConversions",
        "-language:higherKinds"
      ),
      libraryDependencies ++= Seq(
        "org.slf4j"       % "slf4j-api"        % "1.7.7",
        "org.slf4j"       % "jcl-over-slf4j"   % "1.7.7",
        "org.slf4j"       % "jul-to-slf4j"     % "1.7.7",
        "org.slf4j"       % "log4j-over-slf4j" % "1.7.7",
        "ch.qos.logback"  % "logback-classic"  % "1.1.2",
        "ch.qos.logback"  % "logback-core"     % "1.1.2",
        "org.scalatest"  %% "scalatest"        % "2.1.7" % "test"
      ),
      testOptions ++= Seq(
        // Avoid "substitute logger" warning from SLF4J when running tests in
        // parallel. See also: stackoverflow.com/questions/7898273#12095245.
        Tests.Setup { cl =>
          cl.loadClass("org.slf4j.LoggerFactory")
            .getMethod("getLogger", cl.loadClass("java.lang.String"))
            .invoke(null, "ROOT")
        }
      )
    )
  )
}
