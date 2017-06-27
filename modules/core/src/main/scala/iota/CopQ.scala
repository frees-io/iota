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

/** A coproduct of type constructors captured by type constructor list `L` */
final class CopQ[LL <: QList, E, Z[_], F[_], A] private[iota](
  val index: Int,
  val value: Any
) {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: CopQ[LL, E, Z, F, A] => (index == other.index) && (value == other.value)
    case _                                 => false
  }

  override def toString: String =
    s"CopQ($value @ $index)"
}

object CopQ {

  type OnlyL[L <: QList] = {
    type Out[E, Z[_], F[_], A] = CopQ[L, E, Z, F, A]
  }

  def unsafeApply[L <: QList, E, Z[_], F[_], A](index: Int, fa: Z[A]): CopQ[L, E, Z, F, A] =
    new CopQ[L, E, Z, F, A](index, fa)

  /** A type class witnessing the ability to inject type constructor `F`
    * into a coproduct of type constructors `G`
    */
  sealed abstract class Inject[T[_, _[_], _[_], _], U[_, _[_], _[_], _]] {
  // sealed abstract class Inject[T[_, _[_], _[_], _], U[_, _[_], _[_], _] <: CopQ[_, _, _[_], _[_], _]] {
  // sealed abstract class Inject[T[_, _[_], _[_], _], U[_, _[_], _[_], _] <: CopQ[_, _, z, f, _] forSome { type z[_]; type f[_] }] {
    def inj [E, Z[_], F[_], A](ta: T[E, Z, F, A]):        U[E, Z, F, A]
    def proj[E, Z[_], F[_], A](ua: U[E, Z, F, A]): Option[T[E, Z, F, A]]
    final def apply  [E, Z[_], F[_], A](ta: T[E, Z, F, A]):        U[E, Z, F, A]  = inj(ta)
    final def unapply[E, Z[_], F[_], A](ua: U[E, Z, F, A]): Option[T[E, Z, F, A]] = proj(ua)
  }

  object Inject {
    def apply[
      T[_, _[_], _[_], _],
      // U[_, _[_], _[_], _] <: CopQ[_, _, _[_], _[_], _]
      // U[_, _[_], _[_], _] <: CopQ[_, _, z, f, _] forSome { type z[_]; type f[_] }
      U[_, _[_], _[_], _]
    ](implicit ev: Inject[T, U]): Inject[T, U] = ev

    implicit def injectFromInjectL
      [T[_, _[_], _[_], _], L <: QList]
      (implicit ev: InjectL[T, L])
      //   : Inject[T, CopQ[L, ?, ?[_], ?[_], ?]] = 
      // new Inject[T, CopQ[L, ?, ?[_], ?[_], ?]] {
        : Inject[T, CopQ.OnlyL[L]#Out] = 
      new Inject[T, CopQ.OnlyL[L]#Out] {
        def inj [E, Z[_], F[_], A](ta: T[      E, Z, F, A]): CopQ[L,  E, Z, F, A]  = ev.inj(ta)
        def proj[E, Z[_], F[_], A](ca: CopQ[L, E, Z, F, A]): Option[T[E, Z, F, A]] = ev.proj(ca)
      }
  }

  /** A type class witnessing the ability to inject type constructor `T[_, _[_], _[_], _]`
    * into a coproduct of types constructors for [[QList]] type `L`
    */
  final class InjectL[T[_, _[_], _[_], _], L <: QList] private[InjectL](index: Int) {
    def inj [E, Z[_], F[_], A](ta: T[E, Z, F, A]): CopQ[L, E, Z, F, A] =
      new CopQ[L, E, Z, F, A](index, ta)

    def proj[E, Z[_], F[_], A](ca: CopQ[L, E, Z, F, A]): Option[T[E, Z, F, A]] =
      if (ca.index == index) Some(ca.value.asInstanceOf[T[E, Z, F, A]])
      else None

    def apply  [E, Z[_], F[_], A](ta: T[      E, Z, F, A]): CopQ[L,  E, Z, F, A]  = inj(ta)
    def unapply[E, Z[_], F[_], A](ca: CopQ[L, E, Z, F, A]): Option[T[E, Z, F, A]] = proj(ca)
  }

  object InjectL {
    def apply[T[_, _[_], _[_], _], L <: QList](implicit ev: InjectL[T, L]): InjectL[T, L] = ev
    implicit def makeInjectL[T[_, _[_], _[_], _], L <: QList](implicit ev: QList.Pos[L, T]): InjectL[T, L] =
      new InjectL[T, L](ev.index)
  }
}
