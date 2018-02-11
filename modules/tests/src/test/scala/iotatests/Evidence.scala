package iotatests

import iota._             //#=cats
import iota.scalacheck._  //#=cats
import iotaz._            //#=scalaz
import iotaz.scalacheck._ //#=scalaz

import TListH.::
import evidence.FirstH

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
