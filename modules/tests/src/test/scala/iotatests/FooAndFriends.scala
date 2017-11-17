package iotatests

import iota._  //#=cats
import iotaz._ //#=scalaz

object FooAndFriends {
  import TestSingletonLiterals._

  import TList.::

  trait Foo
  type Bar = String
  type Baz = Long with `"baz"`

  type FooBarBazL = Foo :: Bar :: Baz :: TNil
  type BazBarFooL = Baz :: Bar :: Foo :: TNil

  type FooBarBaz = Cop[FooBarBazL]
  type BazBarFoo = Cop[BazBarFooL]


  import TListK.:::

  trait FooK[A]
  type BarK[A] = List[A]
  type BazK[A] = Option[String with A] // ¯\_(ツ)_/¯

  type FooBarBazKL = FooK ::: BarK ::: BazK ::: TNilK
  type BazBarFooKL = BazK ::: BarK ::: FooK ::: TNilK

  type FooBarBazK[A] = CopK[FooBarBazKL, A]
  type BazBarFooK[A] = CopK[BazBarFooKL, A]

}
