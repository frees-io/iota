package iotatests

import iota._  //#=cats
import iotaz._ //#=scalaz

import org.scalacheck._

object ProdHTests {
  import TListH.::
  import TListH.Op._

  trait Foo[F[_]]
  trait Bar[F[_]]
  trait Baz[F[_]] 

  val foo = new Foo[Option] {}
  val bar = new Bar[Option] {}
  val baz = new Baz[Option] {}

  type FooBarBazL = Foo :: Bar :: Baz :: TNilH

  val foo0 = ProdH[FooBarBazL, Option](foo, bar, baz)
  val foo1 = ProdH[Reverse[FooBarBazL], Option](baz, bar, foo)

}