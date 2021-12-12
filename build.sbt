import Wart._

enablePlugins(ScalaJSPlugin)

ThisBuild / scalaVersion     := "2.13.7"
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
    name := "simple-algebraic-subtyping",
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
      JavaSerializable, Serializable, Product,
      LeakingSealed,
      Option2Iterable, TraversableOps,
    ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.3",
    libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.2.7",
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.0.0",
    libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "1.0.0",
    ThisBuild / evictionErrorLevel := Level.Info,
  )

lazy val simplesubJVM = simplesub.jvm
lazy val simplesubJS = simplesub.js
