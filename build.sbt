ThisBuild / scalaVersion := "2.13.0"

val core = pro(project, "core")
  .settings(
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-effect" % "2.0.0",
      "co.fs2" %% "fs2-core" % "2.0.0",
      "org.scala-sbt" % "test-interface" % "1.0",
      "org.scalacheck" %% "scalacheck" % "1.14.0",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.specs2" %% "specs2-core" % "4.7.1"
    ),
    testFrameworks += new TestFramework("klk.KlkFramework"),
  )

val root = pro(project, ".")
  .dependsOn(core)
  .aggregate(core)
  .settings(name := "kallikrein")

import ReleaseTransformations._

releaseCrossBuild := true
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  setReleaseVersion,
  releaseStepCommandAndRemaining("+publish"),
  releaseStepCommand("sonatypeReleaseAll"),
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
)
