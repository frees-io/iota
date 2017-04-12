/* -
 * Iota [iota-core]
 */

package iotatests

import iota._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Shapeless._

object CopTests extends Properties("CopTests") {

  import TList.::

  case class One(a: String)
  case class Two(a: Int)
  case class Three(a: Int)

  type OneTwoThree = One :: Two :: Three :: TNil
  type ThreeTwoOne = Three :: Two :: One :: TNil

  def checkInject[A, L <: TList](
    gen: Gen[A],
    inj: Cop.Inject[A, L],
    index: Int
  ): Prop =
    forAll(gen)(v =>
      inj.inj(v) ?= Cop.Value(index, v))

  property("inject One into OneTwoThree") =
    checkInject(
      arbitrary[One],
      Cop.Inject[One, OneTwoThree],
      0)

  property("inject Two into OneTwoThree") =
    checkInject(
      arbitrary[Two],
      Cop.Inject[Two, OneTwoThree],
      1)

  property("inject Three into OneTwoThree") =
    checkInject(
      arbitrary[Three],
      Cop.Inject[Three, OneTwoThree],
      2)

  property("inject One into ThreeTwoOne") =
    checkInject(
      arbitrary[One],
      Cop.Inject[One, ThreeTwoOne],
      2)

  property("inject Two into ThreeTwoOne") =
    checkInject(
      arbitrary[Two],
      Cop.Inject[Two, ThreeTwoOne],
      1)

  property("inject Three into ThreeTwoOne") =
    checkInject(
      arbitrary[Three],
      Cop.Inject[Three, ThreeTwoOne],
      0)

  type First = Int
  type Last  = String
  type Y

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
    TNil


  property("inject First into Yuge") =
    checkInject(
      arbitrary[First],
      Cop.Inject[First, Yuge],
      0)

  property("inject Last into Yuge") =
    checkInject(
      arbitrary[Last],
      Cop.Inject[Last, Yuge],
      301)
}
