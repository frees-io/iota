package iota  //#=cats
package iotaz //#=scalaz
package internal

import scala.reflect.macros.blackbox.Context

//#+cats
import cats.Traverse
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.either._ //#=2.12
//#-cats

//#+scalaz
import scalaz.Traverse
import scalaz.NonEmptyList
import scalaz.std.list._
//#-scalaz

private[iota]  //#=cats
private[iotaz] //#=scalaz
final class ProductMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def prodApply[L <: TList](args: c.Expr[Any]*)(
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[Prod[L]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L).leftMap(NonEmptyList.one(_))
      argTypes  = args.toList.map(_.tree.tpe)
      _        <- require(argTypes.length == algebras.length,
                    s"Expected ${algebras.length} arguments but received ${argTypes.length}")
      _        <- Traverse[List].traverse(argTypes zip algebras)(tpes =>
                    require(tpes._1 <:< tpes._2,
                      s"Expected ${tpes._1} <:< ${tpes._2}").toAvowal).toEither
    } yield
      q"${tb.iotaPackage}.Prod.unsafeApply[$L](_root_.scala.collection.immutable.IndexedSeq[_root_.scala.Any](..$args))")
  }

  private[this] def require(flag: Boolean, msg: => String): Either[NonEmptyList[String], Unit] =
    Either.cond(flag, (), msg).leftMap(NonEmptyList.one(_))


  def prodHApply[L <: TListH, F[_]](args: c.Expr[Any]*)(
    implicit
      evL: c.WeakTypeTag[L],
      evF: c.WeakTypeTag[F[Nothing]]
  ): c.Expr[ProdH[L, F]] = {

    val L = evL.tpe
    val F = evF.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListHTypes(L).leftMap(NonEmptyList.one(_))
      argTypes  = args.toList.map(_.tree.tpe)
      _        <- require(argTypes.length == algebras.length,
                    s"Expected ${algebras.length} arguments but received ${argTypes.length}")
      _        <- Traverse[List].traverse(argTypes zip algebras)(tpes =>
                    require(appliedType(tpes._1, F) <:< appliedType(tpes._2, F),
                      s"Expected ${appliedType(tpes._1, F)} <:< ${appliedType(tpes._2, F)}").toAvowal).toEither
    } yield
      q"${tb.iotaPackage}.ProdH.unsafeApply[$L, $F](_root_.scala.collection.immutable.IndexedSeq[_root_.scala.Any](..$args))")
  }
}
