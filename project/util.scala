import sbt._

import Keys._

object Util
extends AutoPlugin
{
  object autoImport
  {
    def pro(p: Project, n: String) = p
      .in(file(n))
      .settings(
        name := s"kallikrein-$n",
        addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
        addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
        fork := true,
        scalacOptions ++= List(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-language:higherKinds",
          "-language:experimental.macros",
          "-language:existentials",
          // "-Xfatal-warnings",
          "-Ywarn-value-discard",
          "-Ywarn-unused:imports",
          "-Ywarn-unused:implicits",
          "-Ywarn-unused:params",
          "-Ywarn-unused:patvars",
        )
      )
  }
}
