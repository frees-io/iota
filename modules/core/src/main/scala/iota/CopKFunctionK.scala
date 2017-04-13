/* -
 * Iota [iota-core]
 */

package iota

import cats._
import cats.data._
import cats.instances.all._
import cats.syntax.either._

import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.TypecheckException

class CopKFunctionK[L <: KList, G[_]](
  arrows: Array[Any ~> G]
) extends (CopK[L, ?] ~> G) {
  override def apply[A](ca: CopK[L, A]): G[A] = ca match {
    case CopK.Value(i, v) => arrows(i)(v)
  }
}

object CopKFunctionK {
  def summon[F[a] <: CopK[_, a], G[_]]: F ~> G =
    macro CopKFunctionKMacros.summon[F, G]
}


class CopKFunctionKMacros(val c: Context) {
  import c.universe._

  def summon[F[a] <: CopK[_, a], G[_]](
    implicit
      evF: c.WeakTypeTag[F[_]],
      evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = evF.tpe
    val G = evG.tpe match {
      case TypeRef(_, sym, Nil) => sym
      case tpe => tpe.typeSymbol
    }

    lazy val klists = new SharedKListMacros[c.type](c)

    result(for {
      L    <- destructCopK(F).leftMap(NonEmptyList.of(_))
      tpes <- klists.klistTypesCached(L).leftMap(NonEmptyList.of(_))
      arrs <- Traverse[List].traverse(tpes)(tpe =>
                summonFunctionK(tpe, G)).toEither
    } yield
      q"""
        new cats.arrow.FunctionK[({type F[a] = CopK[$L, a]})#F, $G] {
          private[this] val arrows: Vector[cats.arrow.FunctionK[Any, $G]] =
            Vector[Any](..$arrs).asInstanceOf[Vector[cats.arrow.FunctionK[Any, $G]]]
          override def apply[A](ca: CopK[$L, A]): $G[A] = ca match {
            case CopK.Value(i, v: Any) => arrows(i)(v)
          }
        }
        """)
  }

  def summonFunctionK(F: Type, G: Symbol): ValidatedNel[String, Tree] =
    Validated
      .catchOnly[TypecheckException](
        c.typecheck(q"scala.Predef.implicitly[cats.arrow.FunctionK[$F, $G]]"))
      .leftMap(t => NonEmptyList.of(t.msg))

  def destructCopK(tpe: Type): Either[String, Type] =
    tpe match {
      case TypeRef(_, sym, _) =>
        sym.asType.toType.dealias match {
          case TypeRef(_, _, t :: _ :: Nil) => Right(t)
          case t => Left(s"unespected type $t in destructured $tpe")
        }
      case _ => Left(s"unable to destruct $tpe")
    }

  def result[T](either: Either[NonEmptyList[String], Tree]): c.Expr[T] =
    either fold (
      errors => c.abort(c.enclosingPosition, errors.toList.mkString(", and\n")),
      tree   => c.Expr[T](tree))
}
