val core = pro(project, "core")
  .settings(
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-effect" % "2.0.0-RC1",
      "org.scala-sbt" % "test-interface" % "1.0",
    ),
    testFrameworks += new TestFramework("klk.KlkFramework"),
  )

val root = pro(project, ".")
  .dependsOn(core)
  .aggregate(core)
  .settings(name := "kallikrein")
