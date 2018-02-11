package iotatests

import cats._       //#=cats
import cats.free._  //#=cats
import scalaz._     //#=scalaz
import scalaz.Id.Id //#=scalaz

import org.scalacheck._
import org.scalacheck.Prop._

import iota._  //#=cats
import iotaz._ //#=scalaz

object MathAlgebraKs {
  sealed abstract class AddOne[A]
  object AddOne {
    case class Value(a: Int) extends AddOne[Int]
  }

  sealed abstract class XTwo[A]
  object XTwo {
    case class Value(a: Int) extends XTwo[Int]
  }

  sealed abstract class Neg[A]
  object Neg {
    case class Value(a: Int) extends Neg[Int]
  }

  sealed abstract class Half[A]
  object Half {
    case class Value(a: Int) extends Half[Int]
  }
}

object FreeCopK extends Properties("FreeCopK") {

  import TListK.::
  import MathAlgebraKs._
  type Algebra[A]  = CopK[AddOne :: XTwo :: Neg :: Half :: TNilK, A]

  implicit lazy val evalAddOne: AddOne ~> Id = λ[AddOne ~> Id] { case AddOne.Value(v) => v + 1 }
  implicit lazy val evalXTwo  : XTwo   ~> Id = λ[XTwo   ~> Id] { case XTwo.Value  (v) => v * 2 }
  implicit lazy val evalNeg   : Neg    ~> Id = λ[Neg    ~> Id] { case Neg.Value   (v) => -v    }
  implicit lazy val evalHalf  : Half   ~> Id = λ[Half   ~> Id] { case Half.Value  (v) => v / 2 }

  def liftFree[G[α] <: CopK[_, α]]: LiftFreePartial[G] = new LiftFreePartial[G]

  final class LiftFreePartial[G[α] <: CopK[_, α]] {
    def apply[F[_], A](fa: F[A])(
      implicit I: CopK.Inject[F, G]): Free[G, A] = Free.liftF(I.inj(fa))
  }

  lazy val program: Free[Algebra, Int] =
    for {
      `101`  <- liftFree[Algebra](AddOne.Value(100))
      `-101` <- liftFree[Algebra](Neg.Value(`101`))
      `-202` <- liftFree[Algebra](XTwo.Value(`-101`))
    } yield `-101` + `-202`

  lazy val evalSummon = CopKNT.summon[Algebra, Id]

  property("basic math program through summoned interpreter") =
    program.foldMap(evalSummon) ?= -303

  lazy val evalOf     = CopKNT.of[Algebra, Id](
    evalAddOne, evalXTwo, evalHalf, evalNeg)

  property("basic math program regular interpreter") =
    program.foldMap(evalOf) ?= -303

  object Module {
    type OpTypes = AddOne :: XTwo :: Neg :: Half :: TNilK
    type Op[A] = CopK[OpTypes, A]
  }

  // must compile
  val evalSummonModule = CopKNT.summon[Module.Op, Id]

  // must compile
  val evalOfModule     = CopKNT.of[Module.Op, Id](
    evalAddOne, evalXTwo, evalHalf, evalNeg)
}
