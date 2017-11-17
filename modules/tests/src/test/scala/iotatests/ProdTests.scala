package iotatests

import iota._  //#=cats
import iotaz._ //#=scalaz

import org.scalacheck._

object ProdTests extends Properties("ProdTests") {
  import TList.::
  import TList.Op._

  type StringIntDoubleL = String :: Int :: Double :: TNil

  val foo0 = Prod[StringIntDoubleL]("hello", 20, 6.62607004)
  val foo1 = Prod[Reverse[StringIntDoubleL]](6.62607004, 20, "hello")

}
