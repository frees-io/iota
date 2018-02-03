package iota  //#=cats
package iotaz //#=scalaz
package internal

import evidence._

//#+cats
import cats._
import cats.data._
import cats.instances.all._
import cats.syntax.either._ //#=2.12
import cats.syntax.traverse._
//#-cats

//#+scalaz
import scalaz._
import Scalaz._
//#-scalaz

import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

final class EvidenceMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def materializeAll[L <: TList](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[All[L]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      tpes <- tb.memoizedTListTypes(L).leftMap(NonEmptyList.one)
      evs  <- tpes.traverse(
                summonEvidence(_).toAvowal.leftMap(NonEmptyList.one)).toEither
    } yield
      q"new ${tb.iotaPackage}.evidence.All[$L](${makeProd(L, evs)})")
  }

  def materializeFirstK[L <: TListK, A](
    implicit
      evL: c.WeakTypeTag[L],
      evA: c.WeakTypeTag[A]
  ): c.Expr[FirstK[L, A]] = {

    val L = evL.tpe
    val A = evA.tpe

    type Acc = Either[List[String], (Type, Int, Tree)]

    tb.foldAbort(for {
      tpes <- tb.memoizedTListKTypes(L).leftMap(List(_))
      tup3 <- tpes.foldLeft(Left(Nil): Acc)((acc, F) =>
          acc match {
            case Left(e) =>
              summonEvidence(appliedType(F, A))
                .leftMap(_ :: e)
                .map(fa => (F, e.length, fa))
            case other => other
          })
      (_F, index, fa) = tup3
    } yield
      q"new ${tb.iotaPackage}.evidence.FirstK[$L, $A](${makeCopK(L, _F, A, index, fa)})")
  }

  private[this] def makeProd(
    L: Type,
    values: List[Tree]
  ): Tree =
    q"${tb.iotaPackage}.Prod.unsafeApply[$L]($values)"

  private[this] def makeCopK(
    L: Type,
    F: Type,
    A: Type,
    index: Int,
    fa: Tree
  ): Tree =
    q"${tb.iotaPackage}.CopK.unsafeApply[$L, $F, $A]($index, $fa)"

  private[this] def summonEvidence(T: Type): Either[String, Tree] =
    Avowal
      .catching[TypecheckException](
        c.typecheck(
          q"_root_.scala.Predef.implicitly[$T]"))
      .leftMap(_.msg)
      .toEither

}
