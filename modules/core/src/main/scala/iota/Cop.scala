/* -
 * Iota [iota-core]
 */

package iota

sealed abstract class Cop[L <: TList, A] {
  type Algebras = L
}

object Cop {
  final case class Value[L <: TList, A] private[iota](index: Int, value: A) extends Cop[L, A]

  final class Inject[A, L <: TList] private[Inject](index: Int) {
    def inj(a: A): Cop[L, A] = Cop.Value[L, A](index, a)
    def proj(ca: Cop[L, A]): Option[A] = ca match {
      case Cop.Value(i, v: A @unchecked) if i == index => Some(v)
      case _ => None
    }
  }

  object Inject {
    def apply[A, L <: TList](implicit ev: Inject[A, L]): Inject[A, L] = ev
    implicit def makeInject[A, L <: TList](implicit ev: TList.Pos[L, A]): Inject[A, L] =
      new Inject[A, L](ev.index)
  }
}
