import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS)
  .aggregate(scalacheckJVM, scalacheckJS)
  .aggregate(testsJVM, testsJS)
  .aggregate(examplesCatsJVM, examplesCatsJS)
  .aggregate(examplesScalazJVM, examplesScalazJS)
  .aggregate(bench)
  .aggregate(corezJVM, corezJS)
  .aggregate(scalacheckzJVM, scalacheckzJS)
  .aggregate(testszJVM, testszJS)
  .aggregate(readme, docs)

lazy val core =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/.core"))
    .settings(name := "core")
    .settings(moduleName := "iota-core")
    .settings(macroSettings)
    .settings(yax(file("modules/core/src/main/scala"), Compile,
      flags = "cats" :: Nil,
      yaxScala = true))
    .settings(
      libraryDependencies ++= Seq(
        %%("cats-core", V.cats),
        %%("cats-free", V.cats)))

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val corez =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/.corez"))
    .settings(name := "corez")
    .settings(moduleName := "iotaz-core")
    .settings(macroSettings)
    .settings(yax(file("modules/core/src/main/scala"), Compile,
      flags = "scalaz" :: Nil,
      yaxScala = true))
    .settings(
      libraryDependencies +=
        "org.scalaz" %% "scalaz-core" % V.scalaz)

lazy val corezJVM = corez.jvm
lazy val corezJS = corez.js

lazy val scalacheck =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/.scalacheck"))
    .settings(name := "scalacheck")
    .settings(moduleName := "iota-scalacheck")
    .dependsOn(core)
    .settings(macroSettings)
    .settings(yax(file("modules/scalacheck/src/main/scala"), Compile,
      flags = "cats" :: Nil,
      yaxScala = true))
    .settings(
      libraryDependencies +=
        %%("scalacheck", V.scalacheck))

lazy val scalacheckJVM = scalacheck.jvm
lazy val scalacheckJS = scalacheck.js

lazy val scalacheckz =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/.scalacheckz"))
    .settings(name := "scalacheckz")
    .settings(moduleName := "iotaz-scalacheck")
    .dependsOn(corez)
    .settings(macroSettings)
    .settings(yax(file("modules/scalacheck/src/main/scala"), Compile,
      flags = "scalaz" :: Nil,
      yaxScala = true))
    .settings(
      libraryDependencies +=
        %%("scalacheck", V.scalacheck))

lazy val scalacheckzJVM = scalacheckz.jvm
lazy val scalacheckzJS = scalacheckz.js

lazy val tests =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/.tests"))
    .settings(name := "tests")
    .settings(moduleName := "iota-tests")
    .dependsOn(core)
    .dependsOn(scalacheck)
    .settings(noPublishSettings)
    .settings(macroSettings)
    .settings(yax(file("modules/tests/src/main/scala"), Compile,
      flags = "cats" :: Nil,
      yaxPlatform = true))
    .settings(yax(file("modules/tests/src/test/scala"), Test,
      flags = "cats" :: Nil,
      yaxPlatform = true))
    .settings(
      libraryDependencies ++= Seq(
        %%("scalacheck", V.scalacheck) % "test",
        %%("shapeless", V.shapeless) % "test",
        %%("scheckShapeless", V.scalacheckShapeless) % "test")
    )

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val testsz =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/.testsz"))
    .settings(name := "testsz")
    .settings(moduleName := "iotaz-tests")
    .dependsOn(corez)
    .dependsOn(scalacheckz)
    .settings(noPublishSettings)
    .settings(macroSettings)
    .settings(yax(file("modules/tests/src/main/scala"), Compile,
      flags = "scalaz" :: Nil,
      yaxPlatform = true))
    .settings(yax(file("modules/tests/src/test/scala"), Test,
      flags = "scalaz" :: Nil,
      yaxPlatform = true))
    .settings(
      libraryDependencies ++= Seq(
        %%("scalacheck", V.scalacheck) % "test",
        %%("shapeless", V.shapeless) % "test",
        %%("scheckShapeless", V.scalacheckShapeless) % "test")
    )

lazy val testszJVM = testsz.jvm.settings(
  libraryDependencies += "org.scalatest" %% "scalatest" % V.scalaTest % "test"
)
lazy val testszJS = testsz.js.settings(
  libraryDependencies += "org.scalatest" %%% "scalatest" % V.scalaTest % "test"
)

lazy val examplesCats =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/examples-cats"))
    .settings(name := "examples-cats")
    .settings(moduleName := "iota-examples-cats")
    .dependsOn(core)
    .settings(noPublishSettings)
    .settings(macroSettings)

lazy val examplesCatsJVM = examplesCats.jvm
lazy val examplesCatsJS = examplesCats.js

lazy val examplesScalaz =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/examples-scalaz"))
    .settings(name := "examples-scalaz")
    .settings(moduleName := "iota-examples-scalaz")
    .dependsOn(corez)
    .settings(noPublishSettings)
    .settings(macroSettings)
    .settings(
      libraryDependencies += "org.scalaz" %% "scalaz-effect" % V.scalaz)

lazy val examplesScalazJVM = examplesScalaz.jvm
lazy val examplesScalazJS = examplesScalaz.js

lazy val readme = jvmModule("readme")
  .dependsOn(coreJVM)
  .dependsOn(corezJVM)
  .enablePlugins(TutPlugin)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .settings(
    scalacOptions in Tut := Nil,
    tutTargetDirectory := (baseDirectory in LocalRootProject).value)

lazy val docs = jvmModule("docs")
  .dependsOn(coreJVM)
  .dependsOn(corezJVM)
  .enablePlugins(TutPlugin)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .settings(
    scalacOptions in Tut := Nil,
    tutTargetDirectory := (baseDirectory in LocalRootProject).value / "docs")
  .settings(libraryDependencies +=
    "org.scalaz" %% "scalaz-effect" % V.scalaz)

lazy val bench = jvmModule("bench")
  .enablePlugins(JmhPlugin)
  .dependsOn(coreJVM)
  .configs(Codegen)
  .settings(inConfig(Codegen)(Defaults.configSettings))
  .settings(classpathConfiguration in Codegen := Compile)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .settings(libraryDependencies ++= Seq(
    %%("scalacheck")))
  .settings(inConfig(Compile)(
    sourceGenerators += Def.task {
      val path = (sourceManaged in(Compile, compile)).value / "bench.scala"
      (runner in(Codegen, run)).value.run(
        "iota.bench.BenchBoiler",
        Attributed.data((fullClasspath in Codegen).value),
        path.toString :: Nil,
        streams.value.log)
      path :: Nil
    }
  ))

lazy val Codegen = config("codegen").hide

pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.asc")
pgpSecretRing := file(s"$gpgFolder/secring.asc")

