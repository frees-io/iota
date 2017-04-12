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

  KList.AtPos[OneTwoThree, 1]

  def checkInject[F[_], L <: KList, A](
    gen: Gen[F[A]],
    inj: CopK.Inject[F, L],
    index: Int
  ): Prop =
    forAll(gen)(v =>
      inj.inj(v) ?= CopK.Value(index, v))

  property("inject One into OneTwoThree") =
    checkInject(
      arbitrary[One.Value],
      CopK.Inject[One, OneTwoThree],
      0)

  property("inject Two into OneTwoThree") =
    checkInject(
      arbitrary[Two.Value],
      CopK.Inject[Two, OneTwoThree],
      1)

  property("inject Three into OneTwoThree") =
    checkInject(
      arbitrary[Three.Value],
      CopK.Inject[Three, OneTwoThree],
      2)

  property("inject One into ThreeTwoOne") =
    checkInject(
      arbitrary[One.Value],
      CopK.Inject[One, ThreeTwoOne],
      2)

  property("inject Two into ThreeTwoOne") =
    checkInject(
      arbitrary[Two.Value],
      CopK.Inject[Two, ThreeTwoOne],
      1)

  property("inject Three into ThreeTwoOne") =
    checkInject(
      arbitrary[Three.Value],
      CopK.Inject[Three, ThreeTwoOne],
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
    checkInject(
      arbitrary[First[Int]],
      CopK.Inject[First, Yuge],
      0)

  property("inject Last into Yuge") =
    checkInject(
      arbitrary[Last[Int]],
      CopK.Inject[Last, Yuge],
      301)
}
