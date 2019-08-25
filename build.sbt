ThisBuild / scalaVersion := "2.13.0"

val core = pro(project, "core")
  .settings(
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-effect" % "2.0.0-RC1",
      "co.fs2" %% "fs2-core" % "1.1.0-M1",
      "org.scala-sbt" % "test-interface" % "1.0",
      "org.scalacheck" %% "scalacheck" % "1.14.0",
      "com.chuusai" %% "shapeless" % "2.3.3",
    ),
    testFrameworks += new TestFramework("klk.KlkFramework"),
  )

val root = pro(project, ".")
  .dependsOn(core)
  .aggregate(core)
  .settings(name := "kallikrein")
