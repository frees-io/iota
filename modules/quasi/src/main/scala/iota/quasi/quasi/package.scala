package iota  //#=cats
package iotaz //#=scalaz

import cats._  //#=cats
import scalaz._ //#=scalaz

import TListK.::

package object quasi {

  type Quasi[S[_], A] = quasiImpl.Quasi[S, A]
  type Concur[S[_], A] = quasiImpl.Concur[S, A]
  type Subseq[S[_], A] = quasiImpl.Subseq[S, A]

  implicit final class QuasiOps[S[_], A](val quasi: Quasi[S, A]) extends AnyVal {
    def concur: Concur[S, A] = quasiImpl.toConcur(quasi)
    def subseq: Subseq[S, A] = quasiImpl.toSubseq(quasi)
  }

  final implicit class ConcurOps[S[_], A](val concur: Concur[S, A]) extends AnyVal {
    def quasi: Quasi[S, A] = quasiImpl.fromConcur(concur)
    def subseq: Subseq[S, A] = quasi.subseq

    def ap[B](f: Concur[S, A => B]): Concur[S, B] =
      quasiImpl.ap(f.quasi)(concur.quasi).concur

    def map[B](f: A => B): Concur[S, B] = ap(Quasi.pure(f).concur)
  }

  final implicit class SubseqOps[S[_], A](val subseq: Subseq[S, A]) extends AnyVal {
    def quasi: Quasi[S, A] = quasiImpl.fromSubseq(subseq)
    def concur: Concur[S, A] = quasi.concur

    def map[B](f: A => B): Subseq[S, B] =
      flatMap(a => Quasi.pure(f(a)).subseq)

    def flatMap[B](f: A => Subseq[S, B]): Subseq[S, B] =
      quasiImpl.flatMap(subseq.quasi)(f.andThen(_.quasi)).subseq
  }

  implicit def subseqMonad[S[_]]: Monad[Subseq[S, ?]] = new Monad[Subseq[S, ?]] {
    def pure[A](a: A): Subseq[S, A] = Quasi.pure(a).subseq
    def flatMap[A, B](fa: Subseq[S, A])(f: A => Subseq[S, B]): Subseq[S, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Subseq[S, Either[A, B]]): Subseq[S, B] = ???
  }

  implicit def concurApplicative[S[_]]: Applicative[Concur[S, ?]] = new Applicative[Concur[S, ?]] {
    def pure[A](a: A): Concur[S, A] = Quasi.pure(a).concur
    def ap[A, B](ff: Concur[S, A => B])(fa: Concur[S, A]): Concur[S, B] =
      fa.ap(ff)
  }

  implicit def subseqConcurParallel[S[_]]: Parallel[Subseq[S, ?], Concur[S, ?]] =
    new Parallel[Subseq[S, ?], Concur[S, ?]] {
      val parallel: Subseq[S, ?] ~> Concur[S, ?] =
        λ[Subseq[S, ?] ~> Concur[S, ?]](_.quasi.concur)
      val sequential: Concur[S, ?] ~> Subseq[S, ?] =
        λ[Concur[S, ?] ~> Subseq[S, ?]](_.quasi.subseq)
      val applicative: Applicative[Concur[S, ?]] = Applicative[Concur[S, ?]]
      val monad: Monad[Subseq[S, ?]] = Monad[Subseq[S, ?]]
    }

  object Quasi {

    def pure[S[_], A](a: A): Quasi[S, A] = quasiImpl.pure(a)
    def liftF[S[_], A](value: S[A]): Quasi[S, A] = quasiImpl.suspend(value)

    def toConcur[S[_]]: Quasi[S, ?] ~> Concur[S, ?] =
      λ[Quasi[S, ?] ~> Concur[S, ?]](_.concur)

    def toSubseq[S[_]]: Quasi[S, ?] ~> Subseq[S, ?] =
      λ[Quasi[S, ?] ~> Subseq[S, ?]](_.subseq)

    def foldMap[S[_], M[_], A](quasi: Quasi[S, A])(f: S ~> M)(implicit M: Parallel[M, M]): M[A] =
      quasiImpl.evaluator(f,
        M.parallel, M.sequential,
        M.monad, M.applicative)(quasi)
  }

  private[quasi] sealed trait QuasiImpl {
    type Quasi [S[_], A]
    type Concur[S[_], A]
    type Subseq[S[_], A]

    type Effects[S[_]] =
      Pure    [S,    ?] ::
      Suspend [S,    ?] ::
      FlatMap [S, _, ?] ::
      Ap      [S, _, ?] ::
      Raise   [S, _, ?] ::
      Handle  [S, _, ?] ::
      TNilK

    type Pure[S[_], A] = A
    type Suspend[S[_], A] = S[A]
    final case class FlatMap[S[_], A, B](fa: Quasi[S, A], f: A => Quasi[S, B])
    final case class Ap[S[_], A, B](ff: Quasi[S, A => B], fa: Quasi[S, A])
    type Raise[S[_], E, A] = E
    final case class Handle[S[_], E, A](fe: E => Quasi[S, A])
    //type Handle[S[_], E, A] = E => Quasi[S, A]

    def toRaw[S[_], A](quasi: Quasi[S, A]): CopK[Effects[S], A]
    def fromRaw[S[_], A](copK: CopK[Effects[S], A]): Quasi[S, A]

    def toConcur[S[_], A](quasi: Quasi[S, A]): Concur[S, A]
    def fromConcur[S[_], A](subseq: Concur[S, A]): Quasi[S, A]

    def toSubseq[S[_], A](quasi: Quasi[S, A]): Subseq[S, A]
    def fromSubseq[S[_], A](subseq: Subseq[S, A]): Quasi[S, A]

    def pure[S[_], A](a: A): Quasi[S, A]
    def suspend[S[_], A](value: S[A]): Quasi[S, A]
    def flatMap[S[_], A, B](fa: Quasi[S, A])(f: A => Quasi[S, B]): Quasi[S, B]
    def ap[S[_], A, B](ff: Quasi[S, A => B])(fa: Quasi[S, A]): Quasi[S, B]

    type Evaluator[S[_], M[_]] = Quasi[S, ?] ~> M

    def evaluator[S[_], Zm[_], Za[_]](
      f         : S  ~> Zm,
      parallel  : Zm ~> Za,
      sequential: Za ~> Zm,
      Zm        : Monad[Zm],
      Za        : Applicative[Za]
    ): Evaluator[S, Zm]
  }

  private[quasi] val quasiImpl: QuasiImpl = new QuasiImpl {
    type Quasi [S[_], A] = CopK[Effects[S], A]
    type Concur[S[_], A] = CopK[Effects[S], A]
    type Subseq[S[_], A] = CopK[Effects[S], A]

    def toRaw[S[_], A](quasi: Quasi[S, A]): CopK[Effects[S], A] = quasi
    def fromRaw[S[_], A](copK: CopK[Effects[S], A]): Quasi[S, A] = copK

    def toConcur[S[_], A](quasi: Quasi[S, A]): Concur[S, A] = quasi
    def fromConcur[S[_], A](subseq: Concur[S, A]): Quasi[S, A] = subseq

    def toSubseq[S[_], A](quasi: Quasi[S, A]): Subseq[S, A] = quasi
    def fromSubseq[S[_], A](subseq: Subseq[S, A]): Quasi[S, A] = subseq

    def pure[S[_], A](a: A): Quasi[S, A] =
      CopK.unsafeApply[Effects[S], Pure[S, ?], A](0, a)

    def suspend[S[_], A](value: S[A]): Quasi[S, A] =
      CopK.unsafeApply[Effects[S], Suspend[S, ?], A](1, value)

    def flatMap[S[_], A, B](fa: Quasi[S, A])(f: A => Quasi[S, B]): Quasi[S, B] =
      CopK.unsafeApply[Effects[S], FlatMap[S, A, ?], B](2, FlatMap[S, A, B](fa, f))

    def ap[S[_], A, B](ff: Quasi[S, A => B])(fa: Quasi[S, A]): Quasi[S, B] =
      CopK.unsafeApply[Effects[S], Ap[S, A, ?], B](3, Ap[S, A, B](ff, fa))

    def evaluator[S[_], Zm[_], Za[_]](
      f         : S  ~> Zm,
      parallel  : Zm ~> Za,
      sequential: Za ~> Zm,
      Zm        : Monad[Zm],
      Za        : Applicative[Za]
    ): Evaluator[S, Zm] = new Evaluator[S, Zm] {
      def apply[A](quasi: Quasi[S, A]): Zm[A] =
        Zm.tailRecM(quasi)(q => (q.index: @scala.annotation.switch) match {
          case 0 =>
            val a: A = q.value.asInstanceOf[A]
            Zm.pure(Right(a))
          case 1 =>
            val sa: S[A] = q.value.asInstanceOf[S[A]]
            Zm.map(f(sa))(Right(_))
          case 2 =>
            val n: FlatMap[S, Any, A] = q.value.asInstanceOf[FlatMap[S, Any, A]]
            Zm.map(this(n.fa))(z => Left(n.f(z)))
          case 3 =>
            val n: Ap[S, Any, A] = q.value.asInstanceOf[Ap[S, Any, A]]
            Zm.map(
              sequential(Za.ap(
                parallel(this(n.ff)))(
                parallel(this(n.fa))))
            )(Right(_))
          case _ => scala.Predef.???
        })
    }

  }

}

// example
//#+cats
import cats.implicits._
package quasi {

  object Example {

    def main(args: Array[String]): Unit = {

      trait MathOp[A]
      case class ConstInt(value: Int) extends MathOp[Int]
      case class Add(x: Int, y: Int) extends MathOp[Int]
      case class Neg(x: Int) extends MathOp[Int]

      trait Math[F[_]] { underlying =>
        def const(value: Int): F[Int]
        def add(x: Int, y: Int): F[Int]
        def neg(x: Int): F[Int]

        final def mapK[G[_]](f: F ~> G): Math[G] = new Math[G] {
          def const(value: Int): G[Int] = f(underlying.const(value))
          def add(x: Int, y: Int): G[Int] = f(underlying.add(x, y))
          def neg(x: Int): G[Int] = f(underlying.neg(x))
        }
      }

      object Math {
        def quasi: Math[Quasi[MathOp, ?]] = new Math[Quasi[MathOp, ?]] {
          def const(value: Int): Quasi[MathOp, Int] = Quasi.liftF(ConstInt(value))
          def add(x: Int, y: Int): Quasi[MathOp, Int] = Quasi.liftF(Add(x, y))
          def neg(x: Int): Quasi[MathOp, Int] = Quasi.liftF(Neg(x))
        }

        def concur: Math[Concur[MathOp, ?]] = quasi.mapK[Concur[MathOp, ?]](Quasi.toConcur)
        def subseq: Math[Subseq[MathOp, ?]] = quasi.mapK[Subseq[MathOp, ?]](Quasi.toSubseq)
      }

      val interp: MathOp ~> Id = λ[MathOp ~> Id] {
        case ConstInt(value) => value
        case Add(x, y) => x + y
        case Neg(x) => -x
      }

      val math = Math.subseq

      val program0 = for {
        x <- math.const(1)
        y <- math.const(2)
        z <- math.add(x, y)
      } yield z + 10

      val program1 = for {
        a <- math.const(100)
        b <- math.neg(a)
      } yield a + b

      val program2 = for {
        foo <- math.const(0)
        bar <- List(program0, program1).parSequence
      } yield bar.foldLeft(foo)(_ + _)

      scala.Predef.println(program2)

      val res = Quasi.foldMap(program2.quasi)(interp)
      scala.Predef.println(res)

    }

  }
}
//#-cats
