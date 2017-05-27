/*
 * Copyright 2016-2017 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package iota
package internal

import cats.Applicative
import cats.Functor
import cats.Monad
import cats.Traverse

private[internal] object Recursion {

  // The following code is based on Matryoshka
  // See https://github.com/slamdata/matryoshka

  type Algebra   [F[_], A]       = F[A] =>     A
  type AlgebraM  [M[_], F[_], A] = F[A] =>   M[A]
  type Coalgebra [F[_], A]       =   A  =>   F[A]
  type CoalgebraM[M[_], F[_], A] =   A  => M[F[A]]

  final case class Fix[F[_]](unfix: F[Fix[F]])

  /** Equivalent to ana andThen cata */
  final def hylo[F[_], A, B]
    (a: A)
    (alg: Algebra[F, B], coalg: Coalgebra[F, A])
    (implicit F: Functor[F])
      : B =
    alg(F.map(coalg(a))(hylo(_)(alg, coalg)))

  /** Monadic [[hylo]] */
  final def hyloM[M[_], F[_], A, B]
    (a: A)
    (algM: AlgebraM[M, F, B], coalgM: CoalgebraM[M, F, A])
    (implicit M: Monad[M], F: Traverse[F])
      : M[B] =
    hylo[ λ[α => M[F[α]]], A, M[B] ](a)(
      fb => M.flatMap(fb)(b => M.flatMap(F.sequence(b))(algM)),
      coalgM)(M compose F)

  // the following schemes are fixed for Fix

  /** Unfold */
  final def ana[F[_] : Functor, A]
    (a: A)
    (coalg: Coalgebra[F, A])
      : Fix[F] =
    hylo(a)(Fix(_: F[Fix[F]]), coalg)

  /** Fold */
  final def cata[F[_] : Functor, A]
    (fix: Fix[F])
    (alg: Algebra[F, A])
      : A =
    hylo(fix)(alg, _.unfix)

  /** Monadic [[ana]] */
  final def anaM[M[_], F[_]: Traverse, A]
    (a: A)
    (coalgM: CoalgebraM[M, F, A])
    (implicit M: Monad[M])
      : M[Fix[F]] =
    hyloM[M, F, A, Fix[F]](a)(ffxf => M.pure(Fix(ffxf)), coalgM)

  /** generalize an `Algebra[F, A]` to an `AlgebraM[M, F, A]` */
  implicit final class Algebraops[F[_], A](val alg: Algebra[F, A]) extends AnyVal {
    def generalizeM[M[_]](implicit M: Applicative[M], F: Functor[F]): AlgebraM[M, F, A] =
      node => M.pure(alg(node))
  }

}
