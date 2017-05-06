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

import scala.annotation.tailrec
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

private[internal] final class IotaMacroToolbelt[C <: Context](val c: C)
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

  def foldAbort[T](
    either: Either[String, Tree],
    isImplicit: Boolean = false
  ): c.Expr[T] =
    either fold (
      error => {
        if (isImplicit && showAborts) c.echo(c.enclosingPosition, error)
        c.abort(c.enclosingPosition, error)
      },
      tree  => c.Expr[T](tree))

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

  def memoizedTListTypes(tpe: Type): Either[String, List[Type]] =
    memoize(IotaMacroToolbelt.typeListCache)(tpe, tlistTypes)

  def memoizedKListTypes(tpe: Type): Either[String, List[Type]] =
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

  final lazy val TNilTpe  = typeOf[iota.TNil]
  final lazy val TConsTpe = typeOf[iota.TCons[Nothing, Nothing]].etaExpand.resultType

  final lazy val KNilTpe  = typeOf[iota.KNil]
  final lazy val KConsTpe = typeOf[iota.KCons[Nothing, Nothing]].etaExpand.resultType

  final lazy val CopTpe   = typeOf[iota.Cop[Nothing]].etaExpand.resultType
  final lazy val CopKTpe  = typeOf[iota.CopK[Nothing, Nothing]].etaExpand.resultType

  @tailrec
  final def typeListFoldLeft[A](
    nilTpe: Type, consTpe: Type
  )(tpe: Type)(a0: A)(f: (A, Type) => A): Either[String, A] = tpe.dealias match {
    case TypeRef(_, sym, Nil) if sym.asType.toType <:< nilTpe => Right(a0)
    case TypeRef(_, sym, head :: tail :: Nil) if sym.asType.toType <:< consTpe =>
      typeListFoldLeft(nilTpe, consTpe)(tail)(f(a0, head))(f)
    case _ =>
      Left(s"Unexpected type ${showRaw(tpe)} when inspecting type list")
  }

  final def typeListTypes(
    nilTpe: Type, consTpe: Type
  )(tpe: Type): Either[String, List[Type]] =
    typeListFoldLeft(nilTpe, consTpe)(tpe)(List.empty[Type])((acc, t) => t :: acc).map(_.reverse)

  final def tlistTypes(tpe: Type): Either[String, List[Type]] =
    typeListTypes(TNilTpe, TConsTpe)(tpe)

  final def klistTypes(tpe: Type): Either[String, List[Type]] =
    typeListTypes(KNilTpe, KConsTpe)(tpe)

  final def klistTypeConstructors(tpe: Type): Either[String, List[Type]] =
    klistTypes(tpe).map(_.map(_.etaExpand.resultType))

  case class CopTypes(lType: Type)
  case class CopKTypes(lType: Type, aType: Type)

  private[this] def resultType(sym: Symbol): Type =
    sym.asType.toType.etaExpand.resultType

  final def destructCop(tpe: Type): Either[String, CopTypes] =
    tpe.dealias match {
      case TypeRef(_, sym, l :: Nil) if resultType(sym) <:< CopTpe => Right(CopTypes(l))
      case t => Left(s"unexpected type $t ${showRaw(t)} when destructuring Cop $tpe")
    }

  final def destructCopK(tpe: Type): Either[String, CopKTypes] =
    tpe.dealias match {
      case TypeRef(_, sym, l :: a :: Nil) if resultType(sym) <:< CopKTpe => Right(CopKTypes(l, a))
      case t => Left(s"unexpected type $t when destructuring CopK $tpe")
    }

}
