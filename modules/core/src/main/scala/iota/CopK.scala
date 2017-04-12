/* -
 * Iota [iota-core]
 */

package iota

sealed abstract class CopK[L <: KList, A] {
  type Algebras = L
}

object CopK {
  final case class Value[L <: KList, F[_], A] private[iota](index: Int, value: F[A]) extends CopK[L, A]

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
}
