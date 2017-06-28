import cats.{~>, ApplicativeError, MonadError}
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

  implicit def handler[KL <: KList, M[_], KL2 <: KList](
    implicit 
      M: MonadError[M, E],
      injL: CopK.InjectL[ErrorHandling, KL],
      compute: KList.Compute.Aux[KList.Op.Without[ErrorHandling, KL], KL2],
      restHandler: CopK[KL2, ?] ~> M,
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
            Free.liftF(rest).foldMap[M](compute.apply.andThen(restHandler))
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
  // can't get it implicitly

  // val restHandler = interpretIotaCopK[CopK[FooOp :: BarOp :: KNil, ?], Either[String, ?]]
    // unexpected type [β$12$]iota.CopK[iota.KCons[Example.FooOp,iota.KCons[Example.BarOp,iota.KNil]],β$12$]
    //   PolyType(List(TypeName("β$12$")), TypeRef(ThisType(iota), iota.CopK, List(TypeRef(SingleType(ThisType(iota), iota.package), TypeName("KCons"), List(TypeRef(ThisType(Example), Example.FooOp, List()), TypeRef(SingleType(ThisType(iota), iota.package), TypeName("KCons"), List(TypeRef(ThisType(Example), Example.BarOp, List()), TypeRef(SingleType(SingleType(ThisType(<root>), iota), iota.package), TypeName("KNil"), List()))))), TypeRef(NoPrefix, TypeName("β$12$"), List()))))
    // when destructuring CopK [β$12$]iota.CopK[iota.KCons[Example.FooOp,iota.KCons[Example.BarOp,iota.KNil]],β$12$]

    // wrong number of type arguments for scala.util.Either, should be 2

  // with type aliases -> ok
  type FB[A] = CopK[FooOp :: BarOp :: KNil, A]
  type ErrorOr[A] = Either[String, A]
  val fbHandler = interpretIotaCopK[FB, ErrorOr]

  // val restHandler = CopKFunctionK.summon[CopK[FooOp :: BarOp :: KNil, ?], ErrorOr] // nope
  val restHandler = CopKFunctionK.summon[FB, ErrorOr]
  // val restHandler2 = CopKFunctionK.of[CopK[FooOp :: BarOp :: KNil, ?], ErrorOr](fooHandler, barHandler) // nope
  val restHandler2 = CopKFunctionK.of[FB, ErrorOr](fooHandler, barHandler)
    // wrong number of type arguments for iota.CopK, should be 2

  // handler FBE (FooOp :: BarOp :: ErrorHandling :: KNil)

  val h2: FBE ~> ErrorOr = {
    implicit val fbHandler2: FB ~> ErrorOr = fbHandler
    stringError.handler[FooBarErrorOpL, ErrorOr, FooOp :: BarOp :: KNil]
  }

  // I am not sure if this will be possible ?
  //   (without specifying FooOp :: BarOp :: KNil)
  // val h3: FBE ~> ErrorOr = scala.Predef.implicitly[FBE ~> ErrorOr]
  
  def main(args: Array[String]): Unit = {
    val result = example.foldMap(h2)
    scala.Predef.assert(result == Right(5))
    scala.Predef.println("result = " + result.toString)
  }
}
