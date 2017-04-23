/* -
 * Iota [iota-core]
 */

package iota

import cats.data._

private[iota] class Scala211Compat {
  private[iota] implicit final class EitherCompatOps[A, B](eab: Either[A, B]) {
    def flatMap[AA >: A, C](f: B => Either[AA, C]): Either[AA, C] =
      eab.right.flatMap(f)
    def map[AA >: A, C](f: B => C): Either[AA, C] =
      eab.right.map(f)
    def leftMap[C](f: A => C): Either[C, B] = eab match {
      case Left(a)      => Left(f(a))
      case r @ Right(_) => r.asInstanceOf[Either[C, B]]
    }
    def toValidatedNel[AA >: A]: ValidatedNel[AA, B] = eab match {
      case Left(a)  => Validated.invalidNel(a)
      case Right(b) => Validated.valid(b)
    }
    def filterOrElse[AA >: A](p: B => Boolean, zero: => AA): Either[AA, B] = eab match {
      case Right(b) => if (p(b)) eab else Left(zero)
      case Left(a)  => eab
    }
  }
}
