/* -
 * Iota [iota-core]
 */

package iota

import cats._

import scala.reflect.macros.whitebox.Context

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

    val F = evF.tpe.dealias
    val G = evG.tpe.dealias.typeSymbol

    lazy val klists = new SharedKListMacros[c.type](c)

    result(for {
      L    <- destructCopK(F)
      tpes <- klists.klistTypesCached(L)
    } yield
      q"""
        new cats.arrow.FunctionK[({type F[a] = CopK[$L, a]})#F, $G] {
          private[this] val arrows: Array[cats.arrow.FunctionK[Any, $G]] =
            Array[Any](
              ..${tpes.map(tpe => q"implicitly[cats.arrow.FunctionK[$tpe, $G]]")})
            .asInstanceOf[Array[cats.arrow.FunctionK[Any, $G]]]
          override def apply[A](ca: CopK[$L, A]): $G[A] = ca match {
            case CopK.Value(i, v: Any) => arrows(i)(v)
          }
        }
        """)
  }

  def destructCopK(tpe: Type): Either[String, Type] =
    tpe match {
      case TypeRef(_, sym, _) =>
        sym.asType.toType.dealias match {
          case TypeRef(_, _, t :: _ :: Nil) => Right(t)
          case t => Left(s"unespected type $t in destructured $tpe")
        }
      case _ => Left(s"unable to destruct $tpe")
    }

  def result[T](either: Either[String, Tree]): c.Expr[T] =
    either fold (
      error => c.abort(c.enclosingPosition, error),
      tree  => c.Expr[T](tree))
}
