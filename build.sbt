import org.scalajs.linker.interface.ModuleKind

ThisBuild / scalaVersion := "3.5.2"
ThisBuild / organization := "cpgc"
ThisBuild / version      := "0.1.0"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Wunused:all",
  "-Xkind-projector",
  "-source:3.5"
)

lazy val root = (project in file("."))
  .aggregate(core.jvm, core.js, cli, web)
  .settings(
    name           := "cpgc",
    publish / skip := true
  )

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "cpgc-core",
    // Diagnostics (munit tag "diagnostic") are excluded by default. To run them:
    //   sbt "project coreJVM" "set Test / testOptions := Seq()" "testOnly cpgc.SpacingExperimentSpec"
    // (TopologyEnumerationDiag is JVM-only and writes docs/topology-enumeration.*)
    Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "--exclude-tags=diagnostic"),
    libraryDependencies ++= Seq(
      "org.scalameta"  %%% "munit"           % "1.0.2"  % Test,
      "org.scalacheck" %%% "scalacheck"      % "1.18.1" % Test,
      "org.scalameta"  %%% "munit-scalacheck"% "1.0.0"  % Test
    )
  )

lazy val cli = (project in file("cli"))
  .dependsOn(core.jvm)
  .settings(
    name := "cpgc-cli",
    libraryDependencies ++= Seq(
      "com.monovore" %% "decline" % "2.4.1"
    ),
    Compile / mainClass := Some("cpgc.cli.Main"),
    fork                := true
  )

lazy val web = (project in file("web"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core.js)
  .settings(
    name := "cpgc-web",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.NoModule)),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo"    %%% "laminar"     % "17.2.0"
    )
  )
