import UnidocKeys._

lazy val V = new {
  lazy val cats                     = "0.9.0"
  lazy val scalacheck               = "1.13.4"
  lazy val scalacheckShapeless      = "1.1.3"
}

def module(modName: String): Project =
  Project(modName, file(s"""modules/$modName"""))
    .settings(name := s"iota-$modName")

addCommandAlias("validate", ";" + List(
  "compile",
  "readme/tut", "copyReadme", "checkDiff"
).mkString(";"))

lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .aggregate(`core`)
  .settings(TaskKey[Unit]("copyReadme") := {
    (tutTargetDirectory in `readme`).value.listFiles().foreach(file =>
      IO.copyFile(file, new File((baseDirectory in ThisBuild).value, file.name)))
  })
  .settings(TaskKey[Unit]("checkDiff") := {
    val diff = "git diff".!!
    if (diff.nonEmpty) sys.error("Working directory is dirty!\n" + diff)
  })

lazy val `core` = module("core")
  .settings(macroSettings)
  .settings(crossVersionSharedSources)
  .settings(libraryDependencies ++= Seq(
    "org.typelevel"              %% "cats-core"                 % V.cats,
    "org.typelevel"              %% "cats-free"                 % V.cats
  ))
  .settings(libraryDependencies ++= Seq(
    "org.scalacheck"             %% "scalacheck"                % V.scalacheck,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % V.scalacheckShapeless
  ).map(_ % "test"))

lazy val `readme` = module("readme")
  .dependsOn(`core`)
  .settings(noPublishSettings)
  .settings(tutSettings)
  .settings(
    tutScalacOptions ~= (_.filterNot(Set("-Yno-predef"))))

lazy val macroSettings: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
    "org.typelevel" %% "macro-compat" % "1.1.1",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect.
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Nil
      // in Scala 2.10, quasiquotes are provided by macro paradise.
      case Some((2, 10)) => Seq("org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary)
    }
  }
)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.flatMap { dir: File =>
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, y)) => Some(new File(dir.getPath + "_2.11"))
          case _            => None
        }
      }
    }
  }
