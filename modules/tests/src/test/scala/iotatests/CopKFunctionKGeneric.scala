package iotatests

import cats._   //#=cats
import scalaz._ //#=scalaz

import iota._  //#=cats
import iotaz._ //#=scalaz

object CopKFunctionKGenericTests {

  import TListK.:::

  class X
  class Y

  class T[A]
  class M[F, A]

  type Generic[A] = CopK[M[X, ?] ::: M[Y, ?] ::: TNilK, A]

  val mx: M[X, ?] ~> T = null
  val my: M[Y, ?] ~> T = null
  CopKNT.of[Generic, T](mx, my)

}
