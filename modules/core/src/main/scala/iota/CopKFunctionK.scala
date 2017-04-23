/* -
 * Iota [iota-core]
 */

package iota

import cats._
import cats.data._
import cats.instances.all._

import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.TypecheckException

/** Methods to create [[FunctionK]] instances for [[CopK]] coproducts */
object CopKFunctionK {

  /** Creates a [[FunctionK]] from `F` to `G` by fanning in respective
    * FunctionKs for type all type constructors in the coproduct `F`.
    *
    * The respective FunctionKs are pulled from the input `args` on
    * an as-needed basis; superfluous arguments are ignored.
    */
  def of[F[a] <: CopK[_, a], G[_]](args: Any*): F ~> G =
    macro CopKFunctionKMacros.of[F, G]

  /** Creates a [[FunctionK]] from `F` to `G` by fanning in respective
    * FunctionKs for type all type constructors in the coproduct `F`.
    *
    * The respective FunctionKs are summoned implicitly on an an
    * as-needed basis.
    */
  def summon[F[a] <: CopK[_, a], G[_]]: F ~> G =
    macro CopKFunctionKMacros.summon[F, G]
}

//

final class CopKFunctionKMacros(val c: Context) {
  import c.universe._

  private[this] lazy val klists = new SharedKListMacros[c.type](c)

  def of[F[a] <: CopK[_, a], G[_]](args: c.Expr[Any]*)(
    implicit
      evF: c.WeakTypeTag[F[_]],
      evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = evF.tpe
    val G = evG.tpe match {
      case TypeRef(_, sym, Nil) => sym
      case tpe => tpe.typeSymbol
    }

    result(for {
      L    <- destructCopK(F).leftMap(NonEmptyList.of(_))
      tpes <- klists.klistTypesCached(L).leftMap(NonEmptyList.of(_))
      unorderedPairs <- Traverse[List].traverse(args.toList)(arg =>
        destructFunctionKInput(arg.tree.tpe, evG.tpe.dealias).map((_, arg.tree))).toEither
      lookup = unorderedPairs.toMap
      arrs <- Traverse[List].traverse(tpes)(tpe =>
        lookup.get(tpe).toRight(s"Missing interpreter FunctionK[$tpe, $G]").toValidatedNel).toEither
    } yield makeInterpreter(L, G, arrs))
  }

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

    result(for {
      L    <- destructCopK(F).leftMap(NonEmptyList.of(_))
      tpes <- klists.klistTypesCached(L).leftMap(NonEmptyList.of(_))
      arrs <- Traverse[List].traverse(tpes)(tpe =>
                summonFunctionK(tpe, G)).toEither
    } yield makeInterpreter(L, G, arrs))
  }

  private[this] def makeInterpreter(
    L: Type,
    G: Symbol,
    arrs: List[Tree]
  ): Tree = q"""
    new cats.arrow.FunctionK[({type F[a] = CopK[$L, a]})#F, $G] {
      private[this] val arrows: Vector[cats.arrow.FunctionK[Any, $G]] =
        Vector[Any](..$arrs).asInstanceOf[Vector[cats.arrow.FunctionK[Any, $G]]]
      override def apply[A](ca: CopK[$L, A]): $G[A] = ca match {
        case CopK.Value(i, v: Any) => arrows(i)(v)
      }
    }
    """

  private[this] def summonFunctionK(F: Type, G: Symbol): ValidatedNel[String, Tree] =
    Validated
      .catchOnly[TypecheckException](
        c.typecheck(q"scala.Predef.implicitly[cats.arrow.FunctionK[$F, $G]]"))
      .leftMap(t => NonEmptyList.of(t.msg))

  private[this] def destructCopK(tpe: Type): Either[String, Type] =
    tpe match {
      case TypeRef(_, sym, _) =>
        sym.asType.toType.dealias match {
          case TypeRef(_, _, t :: _ :: Nil) => Right(t)
          case t => Left(s"unespected type $t in destructured $tpe")
        }
      case _ => Left(s"unable to destruct $tpe as CopK")
    }

  private[this] def destructFunctionKInput(tpe: Type, G: Type): ValidatedNel[String, Type] =
    tpe match {
      case TypeRef(_, sym, f :: g :: Nil) if g =:= G => Validated.valid(f)
      case _ => Validated.invalidNel(s"unable to destruct input for $tpe as FunctionK[?, $G]")
    }

  private[this] def result[T](either: Either[NonEmptyList[String], Tree]): c.Expr[T] =
    either fold (
      errors => c.abort(c.enclosingPosition, errors.toList.mkString(", and\n")),
      tree   => c.Expr[T](tree))
}
