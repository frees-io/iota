package iota  //#=cats
package iotaz //#=scalaz

trait TListH

object TListH {

  /** A syntactic sugar alias for [[TConsH]] */
  type ::[H[_[_]], T <: TListH] = TConsH[H, T]

  /** A syntactic sugar alias for [[TConsH]] */
  type :::[H[_[_]], T <: TListH] = TConsH[H, T]

  /** A type class that witnesses the position of `F` in type list `L`
    */
  trait Pos[L <: TListH, H[_[_]]] {
    def index: Int
  }

  object Pos {
    def apply[L <: TListH, F[_[_]]](implicit ev: Pos[L, F]): Pos[L, F] = ev
    implicit def materializePos[L <: TListH, F[_[_]]]: Pos[L, F] =
      macro internal.TypeListMacros.materializeTListHPos[L, F]
  }

  object Op {
    type Concat [L <: TListH, R <: TListH]       <: TListH
    type Reverse[L <: TListH]                    <: TListH
    type Take   [N <: SingletonInt, L <: TListH] <: TListH
    type Drop   [N <: SingletonInt, L <: TListH] <: TListH
    type Remove [K[_[_]], L <: TListH]           <: TListH
  }

  trait Compute[L <: TListH] {
    type Out <: TListH
  }

  object Compute {
    type Aux[L <: TListH, O <: TListH] = Compute[L] { type Out = O }

    def apply[L <: TListH](implicit ev: Compute[L]): Compute.Aux[L, ev.Out] = ev
    implicit def materializeCompute[L <: TListH, O <: TListH]: Aux[L, O] =
      macro internal.TypeListMacros.materializeTListHCompute[L, O]
  }

}
