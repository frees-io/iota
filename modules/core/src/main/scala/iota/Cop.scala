/* -
 * Iota [iota-core]
 */

package iota

/** A coproduct of types captured by type list `L` */
final class Cop[L <: TList] private[iota](
  val index: Int,
  val value: Any
) {
  type Algebras = L

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: Cop[L] => (index == other.index) && (value == other.value)
    case _             => false
  }

  override def toString: String =
    s"Cop($value @ $index)"
}

object Cop {

  def apply[L <: TList, A](index: Int, a: A): Cop[L] =
    new Cop[L](index, a)

  /** A type class witnessing the ability to inject type `A` into a
    * coproduct of types `B`
    */
  sealed abstract class Inject[A, B <: Cop[_]] {
    def inj(a: A): B
    def proj(b: B): Option[A]
    final def apply(a: A): B = inj(a)
    final def unapply(b: B): Option[A] = proj(b)
  }

  object Inject {
    def apply[A, B <: Cop[_]](implicit ev: Inject[A, B]): Inject[A, B] = ev

    implicit def injectFromInjectL[A, L <: TList](
      implicit ev: InjectL[A, L]
    ): Inject[A, Cop[L]] = new Inject[A, Cop[L]] {
      def inj(a: A): Cop[L] = ev.inj(a)
      def proj(c: Cop[L]): Option[A] = ev.proj(c)
    }
  }

  /** A type class witnessing the ability to inject type `A` into a
    * coproduct of types for [[TList]] type `L`
    */
  final class InjectL[A, L <: TList] private[InjectL](index: Int) {
    def inj(a: A): Cop[L] = new Cop[L](index, a)
    def proj(c: Cop[L]): Option[A] =
      if (c.index == index) Some(c.value.asInstanceOf[A])
      else None
    def apply(a: A): Cop[L] = inj(a)
    def unapply(c: Cop[L]): Option[A] = proj(c)
  }

  object InjectL {
    def apply[A, L <: TList](implicit ev: InjectL[A, L]): InjectL[A, L] = ev
    implicit def makeInjectL[A, L <: TList](implicit ev: TList.Pos[L, A]): InjectL[A, L] =
      new InjectL[A, L](ev.index)
  }
}
