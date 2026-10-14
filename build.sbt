import org.scalajs.linker.interface.ModuleKind

ThisBuild / scalaVersion := "3.5.2"
ThisBuild / organization := "cpgc"
ThisBuild / version      := "1.0.3"

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
    //   sbt "project coreJVM" "set Test / testOptions := Seq()" "testOnly cpgc.verify.PerformanceDiag cpgc.verify.TopologyEnumerationDiag"
    // (TopologyEnumerationDiag is JVM-only and writes target/diag/topology-enumeration.*)
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
      "com.monovore" %% "decline" % "2.4.1",
      "org.scalameta" %% "munit" % "1.0.2" % Test
    ),
    Compile / mainClass := Some("cpgc.cli.Main"),
    fork                := true
  )

// Static site for the web UI: index.html + main.js in web/target/dist.
//   sbt web/webDist     optimized build (fullLinkJS), for release
//   sbt web/webDistDev  fast, unoptimized build (fastLinkJS), for development
lazy val webDist    = taskKey[File]("Optimized static site in web/target/dist")
lazy val webDistDev = taskKey[File]("Unoptimized (fast link) static site in web/target/dist")

def copySite(linked: File, resources: File, out: File): File = {
  IO.copyFile(linked / "main.js", out / "main.js")
  IO.copyFile(resources / "index.html", out / "index.html")
  IO.copyDirectory(resources / "vendor", out / "vendor") // vendored three.js, a classic global script (ADR 0019)
  out
}

lazy val web = (project in file("web"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core.js)
  .settings(
    name := "cpgc-web",
    scalaJSUseMainModuleInitializer := true,
    // @nowarn does not silence unused warnings in Scala 3.5, so filter the facades' constructor parameters here
    scalacOptions += "-Wconf:src=.*three/Three[.]scala&msg=unused explicit parameter:s",
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.NoModule)),
    webDist := {
      (Compile / fullLinkJS).value
      copySite((Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value,
        (Compile / resourceDirectory).value, target.value / "dist")
    },
    webDistDev := {
      (Compile / fastLinkJS).value
      copySite((Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value,
        (Compile / resourceDirectory).value, target.value / "dist")
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo"    %%% "laminar"     % "17.2.0",
      "org.scalameta" %%% "munit" % "1.0.2" % Test
    )
  )
