/* -
 * Iota [iota-core]
 */

package iota

import scala.unchecked
import scala.reflect.macros.whitebox.Context
import scala.reflect.ClassTag
import scala.annotation.tailrec

import scala.collection.immutable.Map

object TList {
  type ::[H, T <: TList] = TCons[H, T]

  trait Pos[L <: TList, A] {
    def index: Int
  }

  object Pos {
    def apply[L <: TList, A](implicit ev: Pos[L, A]): Pos[L, A] = ev
    implicit def materializePos[L <: TList, A]: Pos[L, A] =
      macro TListMacros.materializePos[L, A]
  }

  trait AtPos[L <: TList, I <: Int] {
    type Out
  }

  object AtPos {
    type Aux[L <: TList, I <: Int, A] = AtPos[L, I] { type Out = A }
    def apply[L <: TList, I <: Int](implicit ev: AtPos[L, I]): AtPos.Aux[L, I, ev.Out] = ev
    implicit def materializeAtPos[L <: TList, I <: Int, A]: AtPos.Aux[L, I, A] =
      macro TListMacros.materializeAtPos[L, I, A]
  }
}


object TListMacros {
  @volatile private[TListMacros] var klistCache: Map[Any, Any] = Map.empty
}

class TListMacros(val c: Context) {
  import c.universe._

  def materializePos[L <: TList, A](
    implicit
      evL: c.WeakTypeTag[L],
      evA: c.WeakTypeTag[A]
  ): c.Expr[TList.Pos[L, A]] = {

    val L = evL.tpe.dealias
    val A = evA.tpe.dealias

    result(for {
      algebras <- klistTypesCached(L)
      index    <- Right(algebras.indexWhere(_.dealias == A))
                    .filterOrElse(_ >= 0, s"$A is not a member of $L")
    } yield
      q"new TList.Pos[$L, $A]{ override val index: Int = $index }")
  }

  def materializeAtPos[L <: TList, I <: Int, A](
    implicit
      evL: c.WeakTypeTag[L],
      evI: c.WeakTypeTag[I],
      evA: c.WeakTypeTag[A]
  ): c.Expr[TList.AtPos.Aux[L, I, A]] = {

    val L = evL.tpe.dealias
    val I = evI.tpe.dealias

    result(for {
      algebras <- klistTypesCached(L)
      index    <- singletonTypeValue[Int](I)
      tpe      <- algebras.lift(index).toRight(s"index $index out of bounds for type list $algebras")
    } yield
      q"new TList.AtPos[$L, $I] { type Out = $tpe }")
  }

  def result[T](either: Either[String, Tree]): c.Expr[T] =
    either fold (
      error => c.abort(c.enclosingPosition, error),
      tree  => c.Expr[T](tree))

  private[this] val TNilSym          = typeOf[TNil].typeSymbol
  private[this] val TConsSym         = typeOf[TCons[Nothing, Nothing]].typeSymbol

  private[this] def singletonTypeValue[T](tpe: Type)(
    implicit T: ClassTag[T]
  ): Either[String, T] = tpe match {
    case ConstantType(Constant(t: T)) => Right(t)
    case _ => Left(s"$tpe is not a singleton of type $T")
  }

  @tailrec
  private[this] final def klistFoldLeft[A](tpe: Type)(a0: A)(f: (A, Type) => A): Either[String, A] = tpe match {
    case TypeRef(_, TNilSym, Nil) => Right(a0)
    case TypeRef(_, cons, List(headType, tailType)) if cons.asType.toType.contains(TConsSym) =>
      klistFoldLeft(tailType)(f(a0, headType))(f)
    case _ =>
      Left(s"Unexpected type ${showRaw(tpe)} when inspecting HList")
  }

  private[this] final def klistTypes(tpe: Type): Either[String, List[Type]] =
    klistFoldLeft(tpe)(List.empty[Type])((acc, t) => t :: acc).map(_.reverse)

  private[this] final def klistTypesCached(
    tpe: Type
  ): Either[String, List[Type]] = TListMacros.klistCache.synchronized {
    TListMacros.klistCache.get(tpe) match {
      case Some(res: Either[String, List[Type]] @unchecked) =>
        res
      case _ =>
        val res = klistTypes(tpe)
        TListMacros.klistCache += ((tpe, res))
        res
    }
  }
}
