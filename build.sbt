import Wart._

enablePlugins(ScalaJSPlugin)

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.lptk"
ThisBuild / organizationName := "LPTK"

lazy val root = project.in(file("."))
  .aggregate(simplesubJS, simplesubJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val simplesub = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "simple-sub",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:higherKinds",
      "-Ywarn-value-discard",
    ),
    wartremoverWarnings ++= Warts.allBut(
      Recursion, Throw, Nothing, Return, While,
      Var, MutableDataStructures, NonUnitStatements,
      DefaultArguments, ImplicitParameter, StringPlusAny,
      JavaSerializable, Serializable, Product, Any,
      LeakingSealed,
      Option2Iterable,
    ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.0.2",
    libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.3.1",
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "1.0.0",
    ThisBuild / evictionErrorLevel := Level.Info,
  )

lazy val simplesubJVM = simplesub.jvm
lazy val simplesubJS = simplesub.js
