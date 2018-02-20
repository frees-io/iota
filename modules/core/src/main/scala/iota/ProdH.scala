package iota  //#=cats
package iotaz //#=scalaz

import scala.collection.immutable.Seq

final class ProdH[LL <: TListH, F[_]] private(
  val values: Seq[Any]
) {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: ProdH[LL, F] => values == other.values
    case _                   => false
  }

  override def toString: String =
    s"""ProdH(${values.mkString(", ")})"""
}

object ProdH {

  def apply[L <: TListH, F[_]](args: Any*): ProdH[L, F] =
    macro internal.ProductMacros.prodHApply[L, F]

  def unsafeApply[L <: TListH, F[_]](values: Seq[Any]): ProdH[L, F] =
    new ProdH[L, F](values)

}
