//#+jvm

package iotatests

import iota._  //#=cats
import iotaz._ //#=scalaz
import internal.IotaReflectiveToolbelt

import cats.instances.all._      //#=cats
import scalaz.std.AllInstances._ //#=scalaz
import scala.reflect.runtime.{ universe => runtimeUniverse }

import org.scalacheck._

class IotaReflectiveToolbeltTests extends Properties("IotaReflectiveToolbelt") {
  import TypeEqv.syntax._
  import FooAndFriends._

  import runtimeUniverse.{ Type, TypeTag, WeakTypeTag }

  def typeOfK[F[_]](implicit evF: TypeTag[F[Nothing]]): Type = evF.tpe.etaExpand.resultType
  def weakTypeOfK[F[_]](implicit evF: WeakTypeTag[F[Nothing]]): Type = evF.tpe.etaExpand.resultType

  val tb = IotaReflectiveToolbelt(runtimeUniverse)
  import runtimeUniverse.{ weakTypeOf, typeOf }

  property("tlistTypes FooBarBazL") =
    tb.tlistTypes(weakTypeOf[FooBarBazL]) ?=:= Right(
      typeOf[Foo] :: typeOf[Bar] :: typeOf[Baz] :: Nil)

  property("tlistTypes BazBarFooL") =
    tb.tlistTypes(weakTypeOf[BazBarFooL]) ?=:= Right(
      typeOf[Baz] :: typeOf[Bar] :: typeOf[Foo] :: Nil)

  property("tlistkTypeConstructors FooBarBazKL") =
    tb.tlistkTypeConstructors(weakTypeOf[FooBarBazKL]) ?=:= Right(
      typeOfK[FooK] :: typeOfK[BarK] :: typeOfK[BazK] :: Nil)

  property("tlistkTypeConstructors BazBarFooKL") =
    tb.tlistkTypeConstructors(weakTypeOf[BazBarFooKL]) ?=:= Right(
      typeOfK[BazK] :: typeOfK[BarK] :: typeOfK[FooK] :: Nil)

  property("destructCop Cop[FooBarBazL]") =
    tb.destructCop(weakTypeOf[Cop[FooBarBazL]]) ?=:= Right(tb.CopTypes(weakTypeOf[FooBarBazL]))

  property("destructCop FooBarBaz") =
    tb.destructCop(weakTypeOf[FooBarBaz]) ?=:= Right(tb.CopTypes(weakTypeOf[FooBarBazL]))

  property("destructCop Cop[BazBarFooL]") =
    tb.destructCop(weakTypeOf[Cop[BazBarFooL]]) ?=:= Right(tb.CopTypes(weakTypeOf[BazBarFooL]))

  property("destructCop BazBarFoo") =
    tb.destructCop(weakTypeOf[BazBarFoo]) ?=:= Right(tb.CopTypes(weakTypeOf[BazBarFooL]))

  property("destructCopK CopK[FooBarBazKL]") =
    tb.destructCopK(weakTypeOf[CopK[FooBarBazKL, Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[FooBarBazKL], typeOf[Int]))

  property("destructCopK FooBarBazK") =
    tb.destructCopK(weakTypeOf[FooBarBazK[Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[FooBarBazKL], typeOf[Int]))

  property("destructCopK CopK[BazBarFooKL]") =
    tb.destructCopK(weakTypeOf[CopK[BazBarFooKL, Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[BazBarFooKL], typeOf[Int]))

  property("destructCopK BazBarFooK") =
    tb.destructCopK(weakTypeOf[BazBarFooK[Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[BazBarFooKL], typeOf[Int]))
}

//#-jvm
