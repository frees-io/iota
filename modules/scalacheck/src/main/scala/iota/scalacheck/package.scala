package iota  //#=cats
package iotaz //#=scalaz
package scalacheck

import org.scalacheck._

import TList.::
import TList.Op

object `package` {

  implicit def iotaArbitraryCopFromAllProd[L <: TList](
    implicit
      gens   : evidence.All[Op.Map[Arbitrary, L]],
      length : TList.Length[L]
  ): Arbitrary[Cop[L]] =
    Arbitrary(Gen.choose(0, length.value - 1).map(n =>
      Cop.unsafeApply(n, gens.underlying.values(n))))
}
