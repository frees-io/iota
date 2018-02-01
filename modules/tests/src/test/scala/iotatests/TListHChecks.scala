package iotatests

import iota._  //#=cats
import iotaz._ //#=scalaz

import cats._   //#=cats
import scalaz._ //#=scalaz

object TListHChecks {
  import TestSingletonLiterals._

  import TListH.Compute
  import TListH.Op._
  import TListH.::

  def check[L <: TListH, O <: TListH](implicit ev: Compute.Aux[L, O]): Unit = ()

  check[
    Reverse[Functor :: Monad :: TNilH],
    Monad :: Functor :: TNilH]

}
