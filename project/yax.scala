import sbt._
import sbt.Keys._
import java.io._
import scala.io.Source

import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.isScalaJSProject

// Yax preprocessor
// Modified, with permission, from Rob Norris' Doobie
// at https://github.com/tpolecat/doobie

object yax {

  val lineGuard = """(\s*)(.*)\/\/#=([^\s]+)\s*$""".r
  val startGuard = """(\s*)\/\/#\+([^\s]+)\s*$""".r
  val stopGuard = """(\s*)\/\/#-([^\s]+)\s*$""".r

  private def process(file: File, lines: List[String], flags: Set[String]): List[String] = {
    def go(lines: List[(String, Int)], out: List[String], stack: List[String]): List[String] =
      lines match {

        // No more lines, done!
        case Nil =>
          if (stack.isEmpty) out.reverse
          else sys.error(s"$file: EOF: expected ${stack.map(s => s"#-$s").mkString(", ")}")

        // Push a token.
        case (s @ startGuard(leading, tok), _) :: ss =>
          val stack0 = tok :: stack
          val open =
            if (!flags(tok)) s"$leading/*[$tok]"
            else s

          go(ss, open :: out, stack0)

        // Pop a token.
        case (s @ stopGuard(leading, tok), n) :: ss =>
          val line = n + 1
          stack match {
            case `tok` :: ts =>
              val close =
                if (!flags(tok)) s"$leading[$tok]*/"
                else s
              go(ss, close :: out, ts)
            case t :: _      => sys.error(s"$file: $line: expected #-$t, found #-$tok")
            case _           => sys.error(s"$file: $line: unexpected #-$tok")
          }

        // Add a line, or not, depending on tokens.
        case (s, _) :: ss =>
          s match {
            case lineGuard(leading, content, tok) if !flags(tok) =>
              val guarded = s"$leading//[$tok] $content"
              go(ss, guarded :: out, stack)
            case _ =>
              go(ss, s :: out, stack)
          }
      }
    go(lines.zipWithIndex, Nil, Nil)
  }

  def walk(src: File, destDir: File, flags: Set[String]): List[File] =
    if (src.isFile) {
      if (src.isHidden) Nil
      else {
        val f = new File(destDir, src.getName)
        val s = Source.fromFile(src, "UTF-8")
        try {
          destDir.mkdirs()
          val pw = new PrintWriter(f, "UTF-8")
          try {
            process(src, s.getLines.toList, flags).foreach(pw.println)
          } finally {
            pw.close()
          }
        } finally {
          s.close()
        }
        List(f)
      }
    } else {
      try {
        src.listFiles.toList.flatMap(f => walk(f, new File(destDir, src.getName), flags))
      } catch {
        case n: NullPointerException => Nil
      }
    }

  private def foo(root: File, flags: List[String], yaxScala: Boolean, yaxPlatform: Boolean) = Def.task {

    val scalaV = scalaVersion.value
    val isScalaJS = isScalaJSProject.value
    val scalaFlags =
      if (!yaxScala) Nil
      else CrossVersion.partialVersion(scalaV) match {
        case Some((x, y))  => List(s"$x.$y")
        case _             => Nil
      }

    val platformFlags =
      if (yaxPlatform && isScalaJS) List("js")
      else List("jvm")

    val dest = sourceManaged.value
    val sbv = scalaBinaryVersion.value
    val srcs = List(root)
    srcs.flatMap(walk(_, dest, flags.toSet ++ scalaFlags ++ platformFlags))
  }

  // all non-hidden files
  private def closure(src: File): List[File] =
    if (src.isFile) {
      if (src.isHidden) Nil else List(src)
    } else {
      src.listFiles.toList.flatMap(closure)
    }

  def apply(
    root: File,
    config: Configuration,
    flags: List[String] = Nil,
    yaxScala: Boolean = false,
    yaxPlatform: Boolean = false
  ): Seq[Setting[_]] =
    inConfig(config)(Seq(sourceGenerators += foo(root, flags, yaxScala, yaxPlatform).taskValue)) ++
    Seq(watchSources ++= closure(root).get)

}
