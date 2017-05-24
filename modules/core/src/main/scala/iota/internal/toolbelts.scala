/*
 * Copyright 2016-2017 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package iota
package internal

import Recursion._

import cats.{Id, Eval}
import cats.{Applicative, FlatMap, Foldable, Traverse}
import cats.instances.either._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.either._ //#=2.12
import cats.syntax.foldable._
import cats.syntax.functor._

import scala.collection.immutable.Map
import scala.reflect.api.Universe
import scala.reflect.runtime.{ universe => runtimeUniverse }
import scala.reflect.macros.blackbox.Context

private[internal] object IotaMacroToolbelt {
  def apply[C <: Context](c: C): IotaMacroToolbelt[c.type] =
    new IotaMacroToolbelt[c.type](c)

  final class Cache {
    @volatile var underlying: Map[Any, Any] = Map.empty
  }

  final val typeListCache: Cache = new Cache()
}

private[internal] class IotaMacroToolbelt[C <: Context](val c: C)
    extends IotaCommonToolbelt
{
  override type Uu = c.universe.type
  override val u: Uu = c.universe

  import u._

  lazy val showAborts =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowAborts], true).isEmpty

  lazy val showCache =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowCache], true).isEmpty

  lazy val showTrees =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowTrees], true).isEmpty

  def foldAbort[F[_]: Foldable, T](
    either: Either[F[String], Tree],
    isImplicit: Boolean = false
  ): c.Expr[T] =
    either fold (
      errors => {
        val error = errors.toList.mkString(", and\n")
        if (isImplicit && showAborts) c.echo(c.enclosingPosition, error)
        c.abort(c.enclosingPosition, error)
      },
      tree  => {
        if (showTrees) c.echo(c.enclosingPosition, showCode(tree))
        c.Expr[T](tree)
      })

  def memoize[A, B](cache: IotaMacroToolbelt.Cache)(
    a: A, f: A => B
  ): B = cache.underlying.synchronized {
    cache.underlying.get(a) match {
      case Some(b: B @unchecked) =>
        if (showCache)
          c.echo(c.enclosingPosition, s"ShowCache: $b cached result $b")
        b
      case _ =>
        val b = f(a)
        cache.underlying += ((a, b))
        if (showCache)
          c.echo(c.enclosingPosition, s"ShowCache: $a computed result $b")
        b
    }
  }

  def memoizedTListTypes(tpe: Type): Either[Id[String], List[Type]] =
    memoize(IotaMacroToolbelt.typeListCache)(tpe, tlistTypes)

  def memoizedKListTypes(tpe: Type): Either[Id[String], List[Type]] =
    memoize(IotaMacroToolbelt.typeListCache)(tpe, klistTypes)

}

object IotaReflectiveToolbelt {
  def apply[U <: Universe](u: U): IotaReflectiveToolbelt[u.type] =
    new IotaReflectiveToolbelt[u.type](u)

  def apply(): IotaReflectiveToolbelt[runtimeUniverse.type] =
    apply(runtimeUniverse)
}

class IotaReflectiveToolbelt[U <: Universe](override val u: U)
    extends IotaCommonToolbelt { override type Uu = U }

private[internal] sealed abstract class IotaCommonToolbelt {

  type Uu <: Universe
  val u: Uu

  import u._

  object tree {
    sealed trait NodeF[A]
    case class Cons[A](head: Type, tail: A) extends NodeF[A]
    case class Concat[A](nodes: List[A]) extends NodeF[A]
    case class Reverse[A](node: A) extends NodeF[A]
    case class Take[A](n: Int, node: A) extends NodeF[A]
    case class Drop[A](n: Int, node: A) extends NodeF[A]
    case class NNil[A]() extends NodeF[A]

    object Concat { def apply[A](nodes: A*): Concat[A] = Concat(nodes.toList) }
    object Cons {
      def apply[T]: TPartiallyApplied[T] = new TPartiallyApplied[T]

      class TPartiallyApplied[T] {
        def apply[A](tail: A)(implicit evT: WeakTypeTag[T]): Cons[A] = Cons[A](evT.tpe, tail)
      }

      def k[T[_]]: KPartiallyApplied[T] = new KPartiallyApplied[T]()

      class KPartiallyApplied[T[_]] {
        def apply[A](tail: A)(implicit evT: WeakTypeTag[T[_]]): Cons[A] = Cons[A](evT.tpe.typeConstructor, tail)
      }
    }

    object NodeF {
      implicit val nodeTraverse: Traverse[NodeF] = new Traverse[NodeF] {
        def traverse[G[_]: Applicative, A, B](fa: NodeF[A])(f: A => G[B]): G[NodeF[B]] = fa match {
          case Cons(hd, a) => f(a).map(b => Cons(hd, b))
          case Concat(as) => Traverse[List].traverse(as)(f).map(bs => Concat(bs))
          case Reverse(a) => f(a).map(b => Reverse(b))
          case Take(n, a) => f(a).map(b => Take(n, b))
          case Drop(n, a) => f(a).map(b => Drop(n, b))
          case NNil()     => (NNil(): NodeF[B]).pure[G]
        }

        def foldLeft[A, B](fa: NodeF[A], b: B)(f: (B, A) => B): B = fa match {
          case Cons(_, a) => f(b, a)
          case Concat(as) => Foldable[List].foldLeft(as, b)(f)
          case Reverse(a) => f(b, a)
          case Take(_, a) => f(b, a)
          case Drop(_, a) => f(b, a)
          case NNil()     => b
        }

        def foldRight[A, B](fa: NodeF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
          case Cons(_, a) => f(a, lb)
          case Concat(as) => Foldable[List].foldRight(as, lb)(f)
          case Reverse(a) => f(a, lb)
          case Take(_, a) => f(a, lb)
          case Drop(_, a) => f(a, lb)
          case NNil()     => lb
        }
      }
    }
  }

  import tree._

  private[this] def literalInt(tpe: Type): Either[Id[String], Int] =
    tpe match {
      case ConstantType(Constant(value: Int)) => value.asRight
      case _ => s"Expected $tpe to be a literal integer".asLeft
    }

  class TypeListParser private[IotaCommonToolbelt](
    ConsSym   : Symbol,
    NilSym    : Symbol,
    ConcatSym : Symbol,
    ReverseSym: Symbol,
    TakeSym   : Symbol,
    DropSym   : Symbol
  ) {
    private final def coalgH: Type => Either[Id[String], NodeF[Type]] = tpe => tpe.dealias match {
      case TypeRef(_, sym, args) =>
        sym.asType.toType.dealias.typeSymbol match {
          case ConsSym    => Cons(args(0), args(1)).asRight
          case NilSym     => NNil().asRight
          case ConcatSym  => Concat(args(0), args(1)).asRight
          case ReverseSym => Reverse(args(0)).asRight
          case TakeSym    => literalInt(args(0)).map(Take(_, args(1)))
          case DropSym    => literalInt(args(0)).map(Drop(_, args(1)))
          case sym        =>
            s"Unexpected symbol $sym for type $tpe".asLeft
        }
      case ExistentialType(_, res) => coalgH(res)
      case _ => s"Unable to parse type $tpe".asLeft
    }

    final def parse: CoalgebraM[Either[Id[String], ?], NodeF, List[Type]] = {
      case tpe :: Nil => coalgH(tpe).map(_.map(_ :: Nil))
      case tpes => s"Unable to parse type $tpes".asLeft
    }
  }


  private[this] def symbolOf[T](implicit evT: WeakTypeTag[T]): Symbol = evT.tpe.typeSymbol

  final lazy val parseTList: TypeListParser = new TypeListParser(
    NilSym     = symbolOf[iota.TNil],
    ConsSym    = symbolOf[iota.TCons[Nothing, Nothing]],
    ConcatSym  = symbolOf[iota.TList.Op.Concat[Nothing, Nothing]],
    ReverseSym = symbolOf[iota.TList.Op.Reverse[Nothing]],
    TakeSym    = symbolOf[iota.TList.Op.Take[Nothing, Nothing]],
    DropSym    = symbolOf[iota.TList.Op.Drop[Nothing, Nothing]])

  final lazy val parseKList: TypeListParser = new TypeListParser(
    NilSym     = symbolOf[iota.KNil],
    ConsSym    = symbolOf[iota.KCons[Nothing, Nothing]],
    ConcatSym  = symbolOf[iota.KList.Op.Concat[Nothing, Nothing]],
    ReverseSym = symbolOf[iota.KList.Op.Reverse[Nothing]],
    TakeSym    = symbolOf[iota.KList.Op.Take[Nothing, Nothing]],
    DropSym    = symbolOf[iota.KList.Op.Drop[Nothing, Nothing]])


  final def evalTree: Algebra[NodeF, List[Type]] = {
    case Cons(head, types) => head :: types
    case Concat(typeLists) => FlatMap[List].flatten(typeLists)
    case Reverse(types)    => types.reverse
    case Take(n, types)    => types.take(n)
    case Drop(n, types)    => types.drop(n)
    case NNil()            => Nil
  }

  final def tlistTypes(tpe: Type): Either[Id[String], List[Type]] =
    hyloM(tpe :: Nil)(
      evalTree.generalizeM[Either[Id[String], ?]],
      parseTList.parse)

  final def klistTypes(tpe: Type): Either[Id[String], List[Type]] =
    hyloM(tpe :: Nil)(
      evalTree.generalizeM[Either[Id[String], ?]],
      parseKList.parse)

  final def klistTypeConstructors(tpe: Type): Either[Id[String], List[Type]] =
    klistTypes(tpe).map(_.map(_.etaExpand.resultType))

  case class CopTypes(L: Type)
  case class CopKTypes(L: Type, A: Type)

  private[this] final lazy val CopTpe =
    typeOf[iota.Cop[Nothing]].etaExpand.resultType
  private[this] final lazy val CopKTpe =
    typeOf[iota.CopK[Nothing, Nothing]].etaExpand.resultType

  private[this] def resultType(sym: Symbol): Type =
    sym.asType.toType.etaExpand.resultType

  final def destructCop(tpe: Type): Either[Id[String], CopTypes] =
    tpe.dealias match {
      case TypeRef(_, sym, l :: Nil) if resultType(sym) <:< CopTpe => Right(CopTypes(l))
      case TypeRef(_, sym, Nil) => destructCop(sym.asType.toType)
      case t => Left(s"unexpected type $t ${showRaw(t)} when destructuring Cop $tpe")
    }

  final def destructCopK(tpe: Type): Either[Id[String], CopKTypes] =
    tpe.dealias match {
      case TypeRef(_, sym, l :: a :: Nil) if resultType(sym) <:< CopKTpe => Right(CopKTypes(l, a))
      case TypeRef(_, sym, Nil) => destructCopK(sym.asType.toType)
      case t => Left(s"unexpected type $t ${showRaw(t)} when destructuring CopK $tpe")
    }

  class TypeListBuilder private[IotaCommonToolbelt](
    consTpe: Type,
    nilTpe: Type
  ) {
    final def apply(tpes: List[Type]): Type =
      tpes.foldRight(nilTpe)(makeCons)

    private[this] lazy val (consPrefix, consSym) = consTpe match {
      case TypeRef(prefix, sym, _) => (prefix, sym)
      case _ => sys.error("internal iota initialization error")
    }

    private[this] def makeCons(head: Type, tail: Type): Type =
      internal.typeRef(consPrefix, consSym, head :: tail :: Nil)
  }

  final lazy val buildTList: TypeListBuilder =
    new TypeListBuilder(
      weakTypeOf[TCons[_, _]].typeConstructor,
      weakTypeOf[TNil])

  final lazy val buildKList: TypeListBuilder =
    new TypeListBuilder(
      weakTypeOf[KCons[Nothing, _]].typeConstructor,
      weakTypeOf[KNil])

  private[this] def toSymbol(tpe: Type): Symbol = tpe match {
    case TypeRef(_, sym, Nil) => sym
    case _                    => tpe.typeSymbol
  }

  final def defineFastFunctionK(
    className: TypeName,
    F: Type, G: Type,
    preamble: List[Tree],
    toIndex: TermName => Tree,
    handlers: List[TermName => Tree]
  ): Tree = {

    val fa = TermName("fa")
    val A  = TypeName("Ξ")
    val cases = handlers.zipWithIndex
      .map { case (h, i) => cq"$i => ${h(fa)}" }
    val toStringValue = s"FastFunctionK[$F, $G]<<generated>>"

    q"""
    class $className extends _root_.iota.internal.FastFunctionK[$F, $G] {
      ..$preamble
      override def apply[$A]($fa: ${toSymbol(F)}[$A]): ${toSymbol(G)}[$A] =
        (${toIndex(fa)}: @_root_.scala.annotation.switch) match {
          case ..$cases
          case i => throw new _root_.java.lang.Exception(
            s"iota internal error: index " + i + " out of bounds for " + this)
        }
      override def toString: String = $toStringValue
    }
    """
  }

}
