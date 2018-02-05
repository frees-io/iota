package iotatests

import iota._             //#=cats
import iota.scalacheck._  //#=cats
import iotaz._            //#=scalaz
import iotaz.scalacheck._ //#=scalaz

import org.scalacheck._

import TList.::
import TList.Op

object ScalacheckEvidence extends App {

  object Monday
  object Tuesday
  object Wednesday
  object Thursday
  object Friday

  type WeekdayL = Monday.type :: Tuesday.type :: Wednesday.type :: Thursday.type :: Friday.type :: TNil
  type Weekday = Cop[WeekdayL]

  implicit val arbMonday: Arbitrary[Monday.type] = Arbitrary(Gen.const(Monday))
  implicit val arbTuesday: Arbitrary[Tuesday.type] = Arbitrary(Gen.const(Tuesday))
  implicit val arbWednesday: Arbitrary[Wednesday.type] = Arbitrary(Gen.const(Wednesday))
  implicit val arbThursday: Arbitrary[Thursday.type] = Arbitrary(Gen.const(Thursday))
  implicit val arbFriday: Arbitrary[Friday.type] = Arbitrary(Gen.const(Friday))

  implicitly[Arbitrary[Weekday]]
  implicitly[Arbitrary[Cop[WeekdayL]]]
  implicitly[Arbitrary[Cop[Op.Reverse[WeekdayL]]]]
  implicitly[Arbitrary[Cop[Monday.type :: Tuesday.type :: TNil]]]
  implicitly[Arbitrary[Cop[Friday.type :: Tuesday.type :: TNil]]]

}
