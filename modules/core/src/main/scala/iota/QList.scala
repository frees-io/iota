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

/** A heterogenous list of type constructors of type `T[_, _[_], _[_], _]` */
trait QList

object QList {

  /** A syntactic sugar alias for [[QCons]] */
  type ::[H[_, _[_], _[_], _], T <: QList] = QCons[H, T]

  /** A syntactic sugar alias for [[QCons]] */
  type :::[H[_, _[_], _[_], _], T <: QList] = QCons[H, T]

  /** A type class that witnesses the position of type `T` in type
    * "constructor" list `L`
    */
  trait Pos[L <: QList, T[_, _[_], _[_], _]] {
    def index: Int
  }

  object Pos {
    def apply[L <: QList, T[_, _[_], _[_], _]](implicit ev: Pos[L, T]): Pos[L, T] = ev
    implicit def materializePos[L <: QList, T[_, _[_], _[_], _]]: Pos[L, T] =
      macro internal.TypeListMacros.materializeQListPos[L, T]
  }

  object Op {
    type Concat [L <: KList, R <: KList]        <: QList
    type Reverse[L <: KList]                    <: QList
    type Take   [N <: SingletonInt, L <: KList] <: QList
    type Drop   [N <: SingletonInt, L <: KList] <: QList
  }

  trait Compute[L <: QList] {
    type Out <: QList
  }

  object Compute {
    type Aux[L <: QList, O <: QList] = Compute[L] { type Out = O }

    def apply[L <: QList](implicit ev: Compute[L]): Compute.Aux[L, ev.Out] = ev
    implicit def materializeCompute[L <: QList, O <: QList]: Aux[L, O] =
      macro internal.TypeListMacros.materializeQListCompute[L, O]
  }

}
