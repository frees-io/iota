package iota  //#=cats
package iotaz //#=scalaz

import cats.~>   //#=cats
import scalaz.~> //#=scalaz

//#+cats
/** Methods to create [[FunctionK]] instances for [[CopK]] coproducts */
object CopKFunctionK {

  /** Creates a [[FunctionK]] from `F` to `G` by fanning in respective
    * FunctionKs for type all type constructors in the coproduct `F`.
    *
    * The respective FunctionKs are pulled from the input `args` on
    * an as-needed basis; superfluous arguments are ignored.
    */
  def of[F[a] <: CopK[_, a], G[_]](args: Any*): F ~> G =
    macro internal.CopKFunctionKMacros.of[F, G]

  /** Creates a [[FunctionK]] from `F` to `G` by fanning in respective
    * FunctionKs for type all type constructors in the coproduct `F`.
    *
    * The respective FunctionKs are summoned implicitly on an an
    * as-needed basis.
    */
  def summon[F[a] <: CopK[_, a], G[_]]: F ~> G =
    macro internal.CopKFunctionKMacros.summon[F, G]
}
//#-cats

//#+scalaz
/** Methods to create [[NaturalTransformation]] instances for [[CopK]]
  * coproducts */
object CopKNaturalTransformation {

  /** Creates a [[NaturalTransformation]] from `F` to `G` by fanning in
    * respective NaturalTransformations for type all type constructors
    * in the coproduct `F`.
    *
    * The respective NaturalTransformations are pulled from the input
    * `args` on an as-needed basis; superfluous arguments are ignored.
    */
  def of[F[a] <: CopK[_, a], G[_]](args: Any*): F ~> G =
    macro internal.CopKFunctionKMacros.of[F, G]

  /** Creates a [[NaturalTransformation]] from `F` to `G` by fanning in
    * respective NaturalTransformations for type all type constructors
    * in the coproduct `F`.
    *
    * The respective NaturalTransformations are summoned implicitly on
    * an an as-needed basis.
    */
  def summon[F[a] <: CopK[_, a], G[_]]: F ~> G =
    macro internal.CopKFunctionKMacros.summon[F, G]
}
//#-scalaz
