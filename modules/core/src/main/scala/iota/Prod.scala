package iota  //#=cats
package iotaz //#=scalaz

import scala.collection.immutable.Seq

/** A product of types captured by type list `LL` */
final class Prod[LL <: TList] private(
  val values: Seq[Any]
) {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: Prod[LL] => values == other.values
    case _               => false
  }

  override def hashCode(): Int = {
    values.hashCode()
  }

  override def toString: String =
    s"""Prod(${values.mkString(", ")})"""
}

object Prod {

  //#+scalaz
  import scalaz.Isomorphism._
  def gen[A, R <: TList]: A <=> Prod[R] = macro internal.ProductMacros.prodGen[A, R]
  //#-scalaz

  def apply[L <: TList](args: Any*): Prod[L] =
    macro internal.ProductMacros.prodApply[L]

  def unsafeApply[L <: TList](values: Seq[Any]): Prod[L] =
    new Prod[L](values)

}
