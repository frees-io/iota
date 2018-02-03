package iota  //#=cats
package iotaz //#=scalaz
package evidence

/** Implicit evidence helpers for summoning products and coproducts by
  * summing the constituents.
  *
  * Note: These APIs are new and the organization/naming is subject to
  * change across minor release version numbers. The functionality,
  * however, won't be removed.
  */
object `package`

final class All[L <: TList](val underlying: Prod[L]) extends AnyVal

object All {
  def apply[L <: TList](implicit ev: All[L]): All[L] = ev

  implicit def materializeAll[L <: TList]: All[L] =
    macro internal.EvidenceMacros.materializeAll[L]
}


final class FirstK[L <: TListK, A](val underlying: CopK[L, A]) extends AnyVal

object FirstK {
  def apply[L <: TListK, A](implicit ev: FirstK[L, A]): FirstK[L, A] = ev

  implicit def materializeFirstK[L <: TListK, A]: FirstK[L, A] =
    macro internal.EvidenceMacros.materializeFirstK[L, A]

}
