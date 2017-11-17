//#+jvm

package iota  //#=cats
package iotaz //#=scalaz
package internal

import scala.Predef.ArrowAssoc

import org.scalacheck._
import org.scalacheck.Prop._

import cats.instances.either._ //#=cats
import scalaz.std.either._     //#=scalaz
import catryoshka._

import iotatests.FooAndFriends._
import iotatests.TestSingletonLiterals._

object TypeListParserChecks extends Properties("TypeListParsers") {

  val checks = new TypeListParserChecks(IotaReflectiveToolbelt())

  checks.tlists.foreach { case (in, out) =>
    property(s"parse TList $in") = Corecursive[checks.Node].anaM(in)(checks.tb.tlistParser) ?= Right(out) }

  checks.tlistks.foreach { case (in, out) =>
    property(s"parse TListK $in") = Corecursive[checks.Node].anaM(in)(checks.tb.tlistkParser) ?= Right(out) }

}

class TypeListParserChecks(
  override val tb: Toolbelt with TypeListAST with TypeListParsers
) extends TestTreeHelper(tb) {

  import tb.u.Type

  import TList.::
  import TList.Op.{
    Concat  => TConcat,
    Reverse => TReverse,
    Take    => TTake,
    Drop    => TDrop,
    Remove  => TRemove
  }

  val tlists: List[(Type, Node)] = List(
    t[TNil] -> nnil,
    t[Int :: TNil] -> cons[Int](),
    t[String :: Int :: TNil] -> cons[String](cons[Int]()),

    t[TReverse[TNil]] -> reverse(nnil),

    t[Cop[BazBarFooL]#L] -> cons[Baz](cons[Bar](cons[Foo]())),

    t[TConcat[BazBarFoo#L, FooBarBaz#L]] -> concat(
      cons[Baz](cons[Bar](cons[Foo]())),
      cons[Foo](cons[Bar](cons[Baz]()))
    ),

    t[TTake[`0`, BazBarFoo#L]] -> take(0, cons[Baz](cons[Bar](cons[Foo]()))),
    t[TTake[`1`, BazBarFoo#L]] -> take(1, cons[Baz](cons[Bar](cons[Foo]()))),

    t[TDrop[`2`, BazBarFoo#L]] -> drop(2, cons[Baz](cons[Bar](cons[Foo]()))),
    t[TDrop[`3`, BazBarFoo#L]] -> drop(3, cons[Baz](cons[Bar](cons[Foo]()))),

    t[TRemove[Baz, BazBarFoo#L]] -> remove[Baz](cons[Baz](cons[Bar](cons[Foo]()))),
    t[TRemove[Foo, BazBarFoo#L]] -> remove[Foo](cons[Baz](cons[Bar](cons[Foo]())))
  )

  import TListK.Op.{
    Concat  => KConcat,
    Reverse => KReverse,
    Take    => KTake,
    Drop    => KDrop,
    Remove  => KRemove
  }

  val tlistks: List[(Type, Node)] = List(

    t[KReverse[TNilK]] -> reverse(nnil),

    t[CopK[BazBarFooKL, Nothing]#L] -> consk[BazK](consk[BarK](consk[FooK]())),

    t[KConcat[BazBarFooK[_]#L, FooBarBazK[_]#L]] -> concat(
      consk[BazK](consk[BarK](consk[FooK]())),
      consk[FooK](consk[BarK](consk[BazK]()))
    ),

    t[KTake[`0`, BazBarFooK[_]#L]] -> take(0, consk[BazK](consk[BarK](consk[FooK]()))),
    t[KTake[`1`, BazBarFooK[_]#L]] -> take(1, consk[BazK](consk[BarK](consk[FooK]()))),

    t[KDrop[`2`, BazBarFooK[_]#L]] -> drop(2, consk[BazK](consk[BarK](consk[FooK]()))),
    t[KDrop[`3`, BazBarFooK[_]#L]] -> drop(3, consk[BazK](consk[BarK](consk[FooK]()))),

    t[KRemove[BazK, BazBarFooK[_]#L]] -> removek[BazK](consk[BazK](consk[BarK](consk[FooK]()))),
    t[KRemove[FooK, BazBarFooK[_]#L]] -> removek[FooK](consk[BazK](consk[BarK](consk[FooK]())))
  )

}

//#-jvm
