/* -
 * Iota [iota-core]
 */

package iota

import scala.Predef.=:=

import cats.free._

/** A coproduct of type constructors captured by type constructor list `L` */
sealed abstract class CopK[L <: KList, A] {
  type Algebras = L
}

object CopK {

  /** The single value of the coproduct stored with the index of its constructor
    * type in the type list `L`
    */
  final case class Value[L <: KList, F[_], A] private[iota](index: Int, value: F[A]) extends CopK[L, A]

  /** A type class witnessing the ability to inject type constructor `F`
    * into a coproduct of types constructors `L`
    */
  final class Inject[F[_], L <: KList] private[Inject](index: Int) {
    def inj[A](fa: F[A]): CopK[L, A] = CopK.Value[L, F, A](index, fa)
    def proj[A](ca: CopK[L, A]): Option[F[A]] = ca match {
      case CopK.Value(i, v: F[A] @unchecked) if i == index => Some(v)
      case _ => None
    }
  }

  object Inject {
    def apply[F[_], L <: KList](implicit ev: Inject[F, L]): Inject[F, L] = ev
    implicit def makeInject[F[_], L <: KList](implicit ev: KList.Pos[L, F]): Inject[F, L] =
      new Inject[F, L](ev.index)
  }

  def liftFree[G[_]]: LiftFreePartial[G] = new LiftFreePartial[G]

  final class LiftFreePartial[G[_]] private[CopK] {
    def apply[F[_], A, L <: KList](fa: F[A])(
      implicit ev: CopK[L, A] =:= G[A], I: CopK.Inject[F, L]
    ): Free[G, A] = Free.liftF(ev(I.inj(fa)))
  }

  val FunctionK: CopKFunctionK.type = CopKFunctionK
}
