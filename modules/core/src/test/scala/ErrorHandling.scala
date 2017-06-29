import cats.{~>, ApplicativeError, Monad, MonadError}
import cats.free.Free
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.applicativeError._
import iota._
import iota.CopK.Inject

object ErrorProvider {
  def apply[E]: ErrorProvider[E] = new ErrorProvider[E]
}

class ErrorProvider[E] {

  sealed abstract class ErrorHandling[A] extends Product with Serializable
  case class Error[A](e: E) extends ErrorHandling[A]
  case class Recover[CP[_], A](fa: Free[CP, A], f: E => Free[CP, A]) extends ErrorHandling[A]

  implicit def applicativeErrorFreeWithErrorHandling[CP[_] <: CopK[_, _]](
    implicit inj: Inject[ErrorHandling, CP]
  ): ApplicativeError[Free[CP, ?], E] = 
    new ApplicativeError[Free[CP, ?], E]{
      def pure[A](a: A):       Free[CP, A] = Free.pure(a)
      def raiseError[A](e: E): Free[CP, A] = Free.liftF(inj.inj(Error[A](e)))
      def handleErrorWith[A](fa: Free[CP, A])(f: E => Free[CP, A]): Free[CP, A] =
        Free.liftF(inj.inj(Recover(fa, f)))
      def ap[A, B](ff: Free[CP, A => B])(fa: Free[CP, A]): Free[CP, B] =
        ff.flatMap(f => fa.map(f))
    }

  implicit def handler[KL <: KList, M[_]](
    implicit
      M: MonadError[M, E],
      injL: CopK.InjectL[ErrorHandling, KL],
      restHandler: CopK[KList.Op.Without[ErrorHandling, KL], ?] ~> M
  ): CopK[KL, ?] ~> M = 
    new (CopK[KL, ?] ~> M) {
      def apply[A](c: CopK[KL, A]): M[A] = 
        injL.projEither(c) match {
          case Right(errorOp) =>
            errorOp match {
              case Error(e) => M.raiseError(e)
              case r @ Recover(_, _) =>
                val Recover(fa, f) = r.asInstanceOf[Recover[CopK[KL, ?], A]]
                M.handleErrorWith(fa.foldMap(this))(f(_).foldMap(this))
            }
          case Left(rest) =>
            Free.liftF(rest).foldMap(restHandler)
        }
    }

  implicit def interpretIotaCopK[F[a] <: CopK[_, a], G[_]]: F ~> G =
    macro iota.internal.CopKFunctionKMacros.summon[F, G]
}

object Example {

  // other algebras
  sealed abstract class FooOp[A] extends Product with Serializable
  final case class Foo(i: Int) extends FooOp[Int]

  sealed abstract class BarOp[A] extends Product with Serializable
  final case class Bar(s: String) extends BarOp[Int]

  // using String as error type
  val stringError = ErrorProvider[String]
  import stringError._

  // combined coproduct
  import iota.KList.::
  type FooBarErrorOpL = FooOp :: BarOp :: ErrorHandling :: KNil
  type FBE[A] = CopK[FooBarErrorOpL, A]

  // smart constructors
  def foo(i: Int   ): Free[FBE, Int] = Free.liftF(CopK.Inject[FooOp, FBE].inj(Foo(i)))
  def bar(s: String): Free[FBE, Int] = Free.liftF(CopK.Inject[BarOp, FBE].inj(Bar(s)))

  // example program
  val example: Free[FBE, Int] = foo(1).handleErrorWith(e => bar("hello"))

  // handlers simple algebras
  implicit val fooHandler = λ[FooOp ~> Either[String, ?]]{ case Foo(i) => Either.left("kaboom") }
  implicit val barHandler = λ[BarOp ~> Either[String, ?]]{ case Bar(s) => Either.right(s.length) }

  // handler CopK[FooOp :: BarOp :: KNil, ?] ~> Either[String, ?]
  val fbHandler: CopK[FooOp :: BarOp :: KNil, ?] ~> Either[String, ?] = scala.Predef.implicitly

  // handler FBE (FooOp :: BarOp :: ErrorHandling :: KNil)
  val fbeHandler: FBE ~> Either[String, ?] = scala.Predef.implicitly

  // mimick FreeS.interpret
  implicit class FreeInterpret[F[_], A](val fa: Free[F, A]) extends AnyVal {
    def interpret[M[_]: Monad](implicit fk: F ~> M): M[A] = fa.foldMap(fk)
  }
  
  def main(args: Array[String]): Unit = {
    val result = example.interpret[Either[String, ?]]
    scala.Predef.assert(result == Right(5))
    scala.Predef.println("result = " + result.toString)
  }
}
