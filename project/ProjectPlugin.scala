import sbt.Keys._
import sbt._
import sbtorgpolicies.OrgPoliciesPlugin
import sbtorgpolicies.model._
import sbtorgpolicies.OrgPoliciesKeys.orgBadgeListSetting
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.templates.badges._
import sbtorgpolicies.templates._
import scoverage.ScoverageKeys._
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._
import com.timushev.sbt.updates.UpdatesPlugin.autoImport._

object ProjectPlugin extends AutoPlugin {

  override def requires: Plugins = OrgPoliciesPlugin

  override def trigger: PluginTrigger = allRequirements

  object autoImport {

    lazy val V = new {
      val cats: String = "2.0.0"
      val kindProjector: String = "0.10.3"
      val paradise: String = "2.1.1"
      val scala212: String = "2.12.10"
      val scalacheck: String = "1.14.0"
      val scalacheckShapeless: String = "1.2.3"
      val scalaTest: String = "3.0.8"
      val scalaz: String = "7.2.28"
      val shapeless: String = "2.3.3"
    }

    def jvmModule(modName: String): Project =
      Project(modName, file(s"""modules/$modName"""))
        .settings(moduleName := s"iota-$modName")

    lazy val macroSettings: Seq[Setting[_]] = Seq(
      libraryDependencies ++= Seq(
        scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
        scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
        compilerPlugin("org.scalamacros" % "paradise" % V.paradise cross CrossVersion.patch)))
  }

  lazy val commandAliases: Seq[Def.Setting[_]] = addCommandAlias("tutReadme", ";project readme;tut;project root")

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    sharedReleaseProcess ++ warnUnusedImport ++ commandAliases ++ Seq(
      description := "fast product/coproduct types",
      startYear := Option(2016),
      name := "iota",
      orgProjectName := "Iota",
      orgGithubSetting := GitHubSettings(
        organization = "frees-io",
        project = (name in LocalRootProject).value,
        organizationName = "47 Degrees",
        groupId = "io.frees",
        organizationHomePage = url("http://47deg.com"),
        organizationEmail = "hello@47deg.com"
      ),

      orgBadgeListSetting := List(
        TravisBadge.apply(_),
        MavenCentralBadge.apply(_),
        LicenseBadge.apply(_),
        ScalaLangBadge.apply(_),
        ScalaJSBadge.apply(_),
        GitHubIssuesBadge.apply(_)
      ),

      orgUpdateDocFilesSetting +=
        (baseDirectory in LocalRootProject).value / "modules" / "readme" / "src" / "main" / "tut",
      orgEnforcedFilesSetting := List(
        LicenseFileType(orgGithubSetting.value, orgLicenseSetting.value, startYear.value),
        ContributingFileType(orgProjectName.value, orgGithubSetting.value),
        VersionSbtFileType,
        ChangelogFileType,
        ReadmeFileType(
          orgProjectName.value,
          orgGithubSetting.value,
          startYear.value,
          orgLicenseSetting.value,
          orgCommitBranchSetting.value,
          sbtPlugin.value,
          name.value,
          version.value,
          scalaBinaryVersion.value,
          sbtBinaryVersion.value,
          orgSupportedScalaJSVersion.value,
          orgBadgeListSetting.value
        )
      ),

      headerCreate in Compile := Nil,
      headerCreate in Test := Nil,
      orgMaintainersSetting += Dev("andyscott", Some("Andy Scott (twitter: [@andygscott](https://twitter.com/andygscott))"), Some("andy.g.scott@gmail.com")),

      coverageFailOnMinimum := false,
      //fork in Test := !isScalaJSProject.value,
      parallelExecution in Test := false,
      outputStrategy := Some(StdoutOutput),
      connectInput in run := true,
      cancelable in Global := true,
      crossScalaVersions := List(V.scala212),
      scalaVersion := V.scala212,

      addCompilerPlugin("org.typelevel" % "kind-projector" % V.kindProjector cross CrossVersion.binary),
      dependencyUpdatesFilter -= moduleFilter(organization = "org.eclipse.jetty") |
        moduleFilter(organization = "org.openjdk.jmh") |
        moduleFilter(organization = "pl.project13.scala", name = "sbt-jmh-extras")
    )

}
