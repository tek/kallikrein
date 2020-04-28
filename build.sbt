ThisBuild / scalaVersion := "2.13.2"

crossScalaVersions += "2.12.11"

val core = pro("core")
  .settings(
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-effect" % "2.0.0",
      "org.typelevel" %% "cats-laws" % "2.0.0",
      "co.fs2" %% "fs2-core" % "2.1.0",
      "org.scalacheck" %% "scalacheck" % "1.14.0",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.specs2" %% "specs2-core" % "4.7.1" % Test,
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test,
    ),
  )

val sbtp = pro("sbt")
  .dependsOn(core)
  .settings(
    libraryDependencies ++= List(
      "org.scala-sbt" % "test-interface" % "1.0",
    ),
    testFrameworks += new TestFramework("klk.KlkFramework"),
  )

val http4s = pro("http4s")
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    libraryDependencies ++= List(
      "org.http4s" %% "http4s-blaze-server" % "0.21.0-M5",
      "org.http4s" %% "http4s-blaze-client" % "0.21.0-M5",
      "ch.qos.logback" % "logback-classic" % "1.2.3" % Test,
    ),
  )

val http4sSbt = pro("http4s-sbt")
  .dependsOn(http4s, sbtp)
  .settings(
    libraryDependencies ++= List(
      "ch.qos.logback" % "logback-classic" % "1.2.3" % Test,
    ),
    testFrameworks += new TestFramework("klk.KlkFramework"),
  )

val root = basicProject(project.in(file(".")))
  .aggregate(core, sbtp, http4s, http4sSbt)
  .settings(name := "kallikrein")
  .settings(noPublish)

import ReleaseTransformations._

releaseCrossBuild := true
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  setReleaseVersion,
  releaseStepCommandAndRemaining("+publish"),
  releaseStepCommand("sonatypeReleaseAll"),
  tagRelease,
  setNextVersion,
  commitNextVersion,
)
