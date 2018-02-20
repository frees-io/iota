package iotatests

import iota._             //#=cats
import iota.scalacheck._  //#=cats
import iotaz._            //#=scalaz
import iotaz.scalacheck._ //#=scalaz

import TListH.::
import evidence.{AllH, FirstH}

import cats._            //#=cats
import cats.implicits._  //#=cats
import cats.data._       //#=cats
import scalaz._          //#=scalaz
import Scalaz._          //#=scalaz

object Evidence extends App {

  // TODO: implement an easy to use fold method so that these
  // extractors don't need to be defined
  val EvMonad = CopH.InjectL[Monad, Monad :: Applicative :: TNilH]
  val EvApplicative = CopH.InjectL[Applicative, Monad :: Applicative :: TNilH]

  //#+cats
  // TODO: this works for Scalaz, just need to sort out the right
  // imports and types to demonstrate the same functionality

  def mash[F[_], A: Monoid](fx: F[A], fy: F[A])(
    implicit ev: FirstH[Monad :: Applicative :: TNilH, F]
  ): F[A] = ev.underlying match {
    case EvMonad(evF) =>
      println("using monad evidence")
      implicit val F: Monad[F] = evF
      for {
        x <- fx
        y <- fy
      } yield Monoid[A].combine(x, y)
    case EvApplicative(evF) =>
      println("using applicative evidence")
      implicit val F: Applicative[F] = evF
      (fx, fy) mapN Monoid[A].combine
  }

  // will use applicative evidence
  println(mash[Validated[String, ?], Int](
    Validated.valid(1),
    Validated.valid(10)
  ))

  // will use monad evidence
  println(mash[Either[String, ?], Int](
    Right(1),
    Right(10)
  ))

  //#-cats
}


object EvidenceAllH extends App {

  trait Foo[F[_]] { def get: F[Int] }
  trait Bar[F[_]] { def get: F[Int] }

  implicit val foo = new Foo[Option] { def get = Some(1) }
  implicit val bar = new Bar[Option] { def get = Some(2) }

  object getImplicits {
    // try to get out an instance from AllH implicitly
    implicit def fromAllH[H[_[_]], F[_], L2 <: TListH](
      implicit
        pos: TListH.Pos[L2, H],
        allH: AllH[L2, F]
    ): H[F] = allH.underlying.values(pos.index).asInstanceOf[H[F]]
  }

  def foo[F[_]](implicit allH: AllH[Foo :: Bar :: Apply :: TNilH, F]): F[Int] = {

    // manually

    val prodH = allH.underlying.values
    implicit val a = prodH(0).asInstanceOf[Foo[F]]
    implicit val b = prodH(1).asInstanceOf[Bar[F]]
    implicit val c = prodH(2).asInstanceOf[Apply[F]]

    // doesn't compile with `getImplicits`

    // import getImplicits._

    // cause ??
    // not unified in materializeTListHPos ?

    // [info] /Users/peterneyens/dev/iota/modules/.tests/.jvm/target/scala-2.12/src_managed/test/scala/iotatests/Evidence.scala:84:10: TListH.this.Pos.materializePos is not a valid implicit value for iota.TListH.Pos[L,H] because:
    // [info] hasMatchingSymbol reported error: Unexpected symbol type L for type L: TypeRef(NoPrefix, TypeName("L"), List())
    // [info]     Apply[F].map2(implicitly[Foo[F]].get, implicitly[Bar[F]].get)(_ + _)
    // [info]          ^

    val applyF: Apply[F] = getImplicits.fromAllH[Apply, F, Foo :: Bar :: Apply :: TNilH]
    Apply[F]

    Apply[F].map2(implicitly[Foo[F]].get, implicitly[Bar[F]].get)(_ + _)
  }

  println(foo[Option])

}
