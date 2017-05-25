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

import cats.{ Applicative, Functor, Monad, Traverse }
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

// based on Matryoshka (https://github.com/slamdata/matryoshka)
object Recursion {

  type Algebra[F[_], A] = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]
  type AlgebraM[M[_], F[_], A] = F[A] => M[A]
  type CoalgebraM[M[_], F[_], A] = A => M[F[A]]

  case class Fix[F[_]](unfix: F[Fix[F]])

  /** Equivalent to ana andThen cata */
  def hylo[F[_]: Functor, A, B](a: A)(algebra: Algebra[F, B], coalgebra: Coalgebra[F, A]): B =
    algebra(coalgebra(a).map(aa => hylo(aa)(algebra, coalgebra)))

  /** Monadic [[hylo]] */
  def hyloM[M[_], F[_], A, B]
    (a: A)
    (f: AlgebraM[M, F, B], g: CoalgebraM[M, F, A])
    (implicit M: Monad[M], F: Traverse[F])
      : M[B] =
    hylo[λ[α => M[F[α]]], A, M[B]](a)(_ flatMap (_.sequence flatMap f), g)(M compose F)

  // the following schemes are fixed for Fix

  /** Unfold */
  def ana[F[_]: Functor, A](a: A)(f: Coalgebra[F, A]): Fix[F] =
    hylo(a)((fxf: F[Fix[F]]) => Fix[F](fxf), f)

  /** Fold */
  def cata[F[_]: Functor, A](fix: Fix[F])(f: Algebra[F, A]): A =
    hylo(fix)(f, (_: Fix[F]).unfix)

  /** Monadic [[ana]] */
  def anaM[M[_]: Monad, F[_]: Traverse, A](a: A)(f: CoalgebraM[M, F, A]): M[Fix[F]] =
    hyloM[M, F, A, Fix[F]](a)(Fix[F](_).pure[M], f)

  /** generalize an `Algebra[F, A]` to an `AlgebraM[M, F, A]` */
  implicit class Algebraops[F[_], A](val alg: Algebra[F, A]) extends AnyVal {
    def generalizeM[M[_]: Applicative](implicit F: Functor[F]): AlgebraM[M, F, A] =
      node => alg(node).pure[M]
  }

}