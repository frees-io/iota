package object iotatests {
  // needed by Scalacheck when no predef is used
  private[iotatests] implicit def identityView[A](a: A): A = a

  private[iotatests] val CopKNT = iota.CopK.FunctionK              //#=cats
  private[iotatests] val CopKNT = iotaz.CopK.NaturalTransformation //#=scalaz
}
