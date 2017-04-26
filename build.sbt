lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS)
  .aggregate(bench)

lazy val core = module("core")
  .settings(scalaMacroDependencies)
  .settings(crossVersionSharedSources)
  .crossDepSettings(commonCrossDeps: _*)

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val readme = jvmModule("readme")
  .dependsOn(coreJVM)
  .settings(noPublishSettings)
  .settings(readmeSettings)

lazy val bench = jvmModule("bench")
  .enablePlugins(JmhPlugin)
  .dependsOn(coreJVM)
  .configs(Codegen)
  .settings(inConfig(Codegen)(Defaults.configSettings))
  .settings(classpathConfiguration in Codegen := Compile)
  .settings(noPublishSettings)
  .settings(libraryDependencies ++= Seq(%%("scheckShapeless")))
  .settings(inConfig(Compile)(
    sourceGenerators += Def.task {
      val path = ((sourceManaged in (Compile, compile)).value / "bench.scala")
      (runner in (Codegen, run)).value.run(
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
