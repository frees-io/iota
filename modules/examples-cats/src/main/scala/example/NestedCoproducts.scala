package example

import iota._

import scala.Predef._

object NestedCoproducts extends App {

  case class Op0[A]()
  case class Op1[A]()
  case class Op2[A]()
  case class Op3[A]()
  case class Op4[A]()
  case class Op5[A]()

  import TListK.::
  import TListK.Op.Concat
  import TListK.Op.Reverse

  type AlgA[A] = CopK[Op0 :: Op1 :: Op2 :: TNilK, A]
  type AlgB[A] = CopK[Op3 :: Op4 :: Op5 :: TNilK, A]

  type AFwd[A] = CopK[Concat[AlgA[_]#L, AlgB[_]#L], A]
  type ARev[A] = CopK[Reverse[Concat[AlgA[_]#L, AlgB[_]#L]], A]

  implicit class InjectAny[F[_], A](val fa: F[A]) extends AnyVal {
    def inject[G[_] <: CopK[_, _]](implicit ev: CopK.Inject[F, G]): G[A] = ev(fa)
  }

  // ensure that everything gets injected to the right positions
  assert(Op0().inject[AlgA].index == 0)
  assert(Op0().inject[AFwd].index == 0)
  assert(Op0().inject[ARev].index == 5)
  assert(Op1().inject[AlgA].index == 1)
  assert(Op1().inject[AFwd].index == 1)
  assert(Op1().inject[ARev].index == 4)
  assert(Op2().inject[AlgA].index == 2)
  assert(Op2().inject[AFwd].index == 2)
  assert(Op2().inject[ARev].index == 3)
  assert(Op3().inject[AlgB].index == 0)
  assert(Op3().inject[AFwd].index == 3)
  assert(Op3().inject[ARev].index == 2)
  assert(Op4().inject[AlgB].index == 1)
  assert(Op4().inject[AFwd].index == 4)
  assert(Op4().inject[ARev].index == 1)
  assert(Op5().inject[AlgB].index == 2)
  assert(Op5().inject[AFwd].index == 5)
  assert(Op5().inject[ARev].index == 0)

}
