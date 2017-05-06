/*
 * Copyright 2016-2017 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//#+jvm

package iotatests

import iota._
import iota.internal.IotaReflectiveToolbelt

import cats._
import cats.instances.all._

import scala.reflect.runtime.{ universe => runtimeUniverse }

import org.scalacheck._
import org.scalacheck.Prop._
import shapeless._
import shapeless.ops.hlist.{ ToList => HListToList }

class IotaReflectiveToolbeltTests extends Properties("IotaReflectiveToolbelt") {
  import IotaReflectiveToolbeltTests._
  import TList.::
  import KList.:::

  val tb = IotaReflectiveToolbelt(runtimeUniverse)
  import runtimeUniverse._

  trait Foo
  trait Bar
  trait Baz

  type FooBarBazL = Foo :: Bar :: Baz :: TNil
  type BazBarFooL = Baz :: Bar :: Foo :: TNil

  property("tlistTypes FooBarBazL") =
    tb.tlistTypes(weakTypeOf[FooBarBazL]) ?=:= Right(
      typeOf[Foo] :: typeOf[Bar] :: typeOf[Baz] :: Nil)

  property("tlistTypes BazBarFooL") =
    tb.tlistTypes(weakTypeOf[BazBarFooL]) ?=:= Right(
      typeOf[Baz] :: typeOf[Bar] :: typeOf[Foo] :: Nil)

  trait FooK[A]
  trait BarK[A]
  trait BazK[A]

  type FooBarBazKL = FooK ::: BarK ::: BazK ::: KNil
  type BazBarFooKL = BazK ::: BarK ::: FooK ::: KNil

  property("klistTypeConstructors FooBarBazKL") =
    tb.klistTypeConstructors(weakTypeOf[FooBarBazKL]) ?=:= Right(
      typeOfK[FooK] :: typeOfK[BarK] :: typeOfK[BazK] :: Nil)

  property("klistTypeConstructors BazBarFooKL") =
    tb.klistTypeConstructors(weakTypeOf[BazBarFooKL]) ?=:= Right(
      typeOfK[BazK] :: typeOfK[BarK] :: typeOfK[FooK] :: Nil)


  type FooBarBaz = Cop[FooBarBazL]
  type BazBarFoo = Cop[BazBarFooL]

  property("destructCop Cop[FooBarBazL]") =
    tb.destructCop(weakTypeOf[Cop[FooBarBazL]]) ?=:= Right(tb.CopTypes(weakTypeOf[FooBarBazL]))

  property("destructCop FooBarBaz") =
    tb.destructCop(weakTypeOf[FooBarBaz]) ?=:= Right(tb.CopTypes(weakTypeOf[FooBarBazL]))

  property("destructCop Cop[BazBarFooL]") =
    tb.destructCop(weakTypeOf[Cop[BazBarFooL]]) ?=:= Right(tb.CopTypes(weakTypeOf[BazBarFooL]))

  property("destructCop BazBarFoo") =
    tb.destructCop(weakTypeOf[BazBarFoo]) ?=:= Right(tb.CopTypes(weakTypeOf[BazBarFooL]))

  type FooBarBazK[A] = CopK[FooBarBazKL, A]
  type BazBarFooK[A] = CopK[BazBarFooKL, A]

  property("destructCopK CopK[FooBarBazKL]") =
    tb.destructCopK(weakTypeOf[CopK[FooBarBazKL, Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[FooBarBazKL], typeOf[Int]))

  property("destructCopK FooBarBazK") =
    tb.destructCopK(weakTypeOf[FooBarBazK[Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[FooBarBazKL], typeOf[Int]))

  property("destructCopK CopK[BazBarFooKL]") =
    tb.destructCopK(weakTypeOf[CopK[BazBarFooKL, Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[BazBarFooKL], typeOf[Int]))

  property("destructCopK BazBarFooK") =
    tb.destructCopK(weakTypeOf[BazBarFooK[Int]]) ?=:= Right(tb.CopKTypes(weakTypeOf[BazBarFooKL], typeOf[Int]))

}

object IotaReflectiveToolbeltTests {
  import runtimeUniverse._

  sealed trait TypeEqv[A] {
    def check(x: A, y: A): Prop
  }

  object TypeEqv extends TypeEqvInstances0

  sealed class TypeEqvInstances0 extends TypeEqvInstances1 {
    implicit val idTypeEqv: TypeEqv[Type] = new TypeEqv[Type] {
      def check(x: Type, y: Type): Prop =
        Prop(x =:= y) :| s"$x was not =:= to $y"
    }

    implicit def eitherTypeEqv[A, B](
      implicit eqv: TypeEqv[B]
    ): TypeEqv[Either[A, B]] = new TypeEqv[Either[A, B]] {
      def check(ex: Either[A, B], ey: Either[A, B]): Prop =
        (ex, ey) match {
          case (Right(bx), Right(by)) => eqv.check(bx, by)
          case _                      => ex ?= ey
        }
    }
  }

  sealed class TypeEqvInstances1 {
    implicit def foldableTypeEqv[F[_], A](
      implicit F: Foldable[F], eqv: TypeEqv[A]
    ): TypeEqv[F[A]] = new TypeEqv[F[A]] {
      def check(fx: F[A], fy: F[A]): Prop =
        (F.toList(fx) zip F.toList(fy)).foldLeft(proved)((acc, vv) =>
          acc && eqv.check(vv._1, vv._2))
    }

    implicit def genericTypeEqv[P, L <: HList, A](
      implicit
        gen: Generic.Aux[P, L],
        toList: HListToList[L, A],
        eqv: TypeEqv[List[A]]
    ): TypeEqv[P] = new TypeEqv[P] {
      def check(x: P, y: P): Prop =
        eqv.check(toList(gen.to(x)), toList(gen.to(y)))
    }
  }

  final implicit class TypeEqvOps[A](x: A)(implicit eqv: TypeEqv[A]) {
    def ?=:=(y: A): Prop = eqv.check(x, y)
  }

  def typeOfK[F[_]](implicit evF: TypeTag[F[Nothing]]): Type = evF.tpe.etaExpand.resultType
  def weakTypeOfK[F[_]](implicit evF: WeakTypeTag[F[Nothing]]): Type = evF.tpe.etaExpand.resultType
}

//#-jvm
