name := "quasimodo"

scalaVersion := "2.12.10"

import Dependencies._

import scala.util.Properties

lazy val root = (project in file("."))
  .aggregate(main)
  .settings(
    skip in publish := true
  )

val main = (project in file("main"))
  .settings(
    libraryDependencies ++= Seq(
      catsEffect,
      expecty,
      scalacheck
    )
  )
  .settings(commonSettings)

lazy val commonScalacOptions = Seq(
  "-unchecked",
  "-deprecation",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding",
  "UTF-8",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused",
  "-Xfatal-warnings",
  "-Xlint",
  "-Ypartial-unification",
  "-Yrangepos"
)

lazy val commonSettings = Def.settings(
  inThisBuild(
    List(
      organization := "com.velvetbeam",
      scalaVersion := "2.12.10"
    )
  ),
  scalafmtOnCompile := false,
  addCommandAlias(
    "validate",
    ";clean; scalafmtCheckAll; test;"
  ),
  scalacOptions ++= commonScalacOptions,
  addCompilerPlugin(kindProjector)
)
