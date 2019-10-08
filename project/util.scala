import sbt._

import Keys._

object Util
extends AutoPlugin
{
  object autoImport
  {
    val github = "https://github.com/tek"
    val projectName = "kallikrein"
    val repo = s"$github/$projectName"

    def noPublish: List[Setting[_]] = List(skip in publish := true)

    def basicProject(p: Project) =
      p.settings(
        organization := "io.tryp",
        addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
        addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
        fork := true,
        scalacOptions ++= List(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-language:higherKinds",
          "-language:existentials",
          "-Xfatal-warnings",
          "-Ywarn-value-discard",
          "-Ywarn-unused:imports",
          "-Ywarn-unused:implicits",
          "-Ywarn-unused:params",
          "-Ywarn-unused:patvars",
        )
      )

    def pro(n: String) =
      basicProject(Project(n, file(n)))
      .settings(
        name := s"$projectName-$n",
        addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
        addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
        publishMavenStyle := true,
        publishTo := Some(
          if (isSnapshot.value) Opts.resolver.sonatypeSnapshots
          else Resolver.url("sonatype staging", url("https://oss.sonatype.org/service/local/staging/deploy/maven2"))
        ),
        licenses := List("BOML" -> url("https://blueoakcouncil.org/license/1.0.0")),
        homepage := Some(url(repo)),
        scmInfo := Some(ScmInfo(url(repo), s"scm:git@github.com:tek/$projectName")),
        developers := List(Developer(id="tek", name="Torsten Schmits", email="torstenschmits@gmail.com",
          url=url(github))),
      )
  }
}
