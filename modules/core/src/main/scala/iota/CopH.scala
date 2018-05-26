package iota  //#=cats
package iotaz //#=scalaz

import cats.~>   //#=cats
import scalaz.~> //#=scalaz

final class CopH[LL <: TListH, F[_]] private(
  val index: Int,
  val value: Any
) extends Serializable {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: CopH[LL, F] => (index == other.index) && (value == other.value)
    case _                  => false
  }

  override def hashCode(): Int = {
    41 * index + value.##
  }

  override def toString: String =
    s"CopH($value @ $index)"
}

object CopH {

  def unsafeApply[L <: TListH, H[_[_]], F[_]](index: Int, hf: H[F]): CopH[L, F] =
    new CopH[L, F](index, hf)

  sealed abstract class Inject[H[_[_]], J[F[_]]  <: CopH[_, F]] {
    def inj[F[_]](hf: H[F]): J[F]
    def prj[F[_]](jf: J[F]): Option[H[F]]
    final def apply[F[_]](hf: H[F]): J[F] = inj(hf)
    final def unapply[F[_]](jf: J[F]): Option[H[F]] = prj(jf)
  }

  object Inject {
    def apply[H[_[_]], J[F[_]] <: CopH[_, F]](implicit ev: Inject[H, J]): Inject[H, J] = ev

    implicit def injectFromInjectL[H[_[_]], L <: TListH](
      implicit ev: InjectL[H, L]
    ): Inject[H, CopH[L, ?[_]]] = new Inject[H, CopH[L, ?[_]]] {
      def inj[F[_]](hf: H[F]): CopH[L, F] = ev.inj(hf)
      def prj[F[_]](jf: CopH[L, F]): Option[H[F]] = ev.proj(jf)
    }
  }

  final class InjectL[H[_[_]], L <: TListH] private[InjectL](index: Int) {
    def inj[F[_]](hf: H[F]): CopH[L, F] = new CopH[L, F](index, hf)
    def proj[F[_]](cf: CopH[L, F]): Option[H[F]] =
      if (cf.index == index) Some(cf.value.asInstanceOf[H[F]])
      else None
    def apply[F[_]](hf: H[F]): CopH[L, F] = inj(hf)
    def unapply[F[_]](cf: CopH[L, F]): Option[H[F]] = proj(cf)
  }

  object InjectL {
    def apply[H[_[_]], L <: TListH](implicit ev: InjectL[H, L]): InjectL[H, L] = ev
    implicit def makeInjectL[H[_[_]], L <: TListH](implicit ev: TListH.Pos[L, H]): InjectL[H, L] =
      new InjectL[H, L](ev.index)
  }

  final class RemoveL[H[_[_]], L <: TListH] private[RemoveL](index: Int) {
    def apply[F[_]](c: CopH[L, F]): Either[CopH[TListH.Op.Remove[H, L], F], H[F]] =
      Either.cond(
        c.index == index,
        c.value.asInstanceOf[H[F]],
        new CopH(if (c.index < index) c.index else c.index - 1, c.value))
  }

  object RemoveL {
    def apply[H[_[_]], L <: TListH](implicit ev: RemoveL[H, L]): RemoveL[H, L] = ev
    implicit def makeRemoveL[H[_[_]], L <: TListH](implicit ev: TListH.Pos[L, H]): RemoveL[H, L] =
      new RemoveL[H, L](ev.index)
  }

}
