/* -
 * Iota [iota-core]
 */

package iotatests

import iota._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Shapeless._

object CopKTests extends Properties("CopKTests") {

  import KList.::

  sealed abstract class One[A]
  object One {
    case class Value(a: String) extends One[Double]
  }

  sealed abstract class Two[A]
  object Two {
    case class Value(a: Int) extends Two[Int]
  }

  sealed abstract class Three[A]
  object Three {
    case class Value(a: Int) extends Three[Int]
  }

  type OneTwoThree = One :: Two :: Three :: KNil
  type ThreeTwoOne = Three :: Two :: One :: KNil

  def checkInjectL[F[_], L <: KList, A](
    gen: Gen[F[A]],
    inj: CopK.InjectL[F, L],
    index: Int
  ): Prop =
    forAll(gen)(v =>
      inj.inj(v) ?= CopK(index, v))

  property("inject One into OneTwoThree") =
    checkInjectL(
      arbitrary[One.Value],
      CopK.InjectL[One, OneTwoThree],
      0)

  property("inject Two into OneTwoThree") =
    checkInjectL(
      arbitrary[Two.Value],
      CopK.InjectL[Two, OneTwoThree],
      1)

  property("inject Three into OneTwoThree") =
    checkInjectL(
      arbitrary[Three.Value],
      CopK.InjectL[Three, OneTwoThree],
      2)

  property("inject One into ThreeTwoOne") =
    checkInjectL(
      arbitrary[One.Value],
      CopK.InjectL[One, ThreeTwoOne],
      2)

  property("inject Two into ThreeTwoOne") =
    checkInjectL(
      arbitrary[Two.Value],
      CopK.InjectL[Two, ThreeTwoOne],
      1)

  property("inject Three into ThreeTwoOne") =
    checkInjectL(
      arbitrary[Three.Value],
      CopK.InjectL[Three, ThreeTwoOne],
      0)

  type First[A] = A
  type Last[A] = A
  type Y[A]

  type Yuge =
    First :: // 20 rows of 15 = 300 filler items
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
    Last ::
    KNil


  property("inject First into Yuge") =
    checkInjectL(
      arbitrary[First[Int]],
      CopK.InjectL[First, Yuge],
      0)

  property("inject Last into Yuge") =
    checkInjectL(
      arbitrary[Last[Int]],
      CopK.InjectL[Last, Yuge],
      301)
}
