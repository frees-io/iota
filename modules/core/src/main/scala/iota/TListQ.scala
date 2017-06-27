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

/** A heterogenous list of type constructors of type "Q": `T[_, _[_], _[_], _]`
  */
trait TListQ

object TListQ {

  /** A syntactic sugar alias for [[TConsQ]] */
  type ::[H[_, _[_], _[_], _], T <: TListQ] = TConsQ[H, T]

  /** A syntactic sugar alias for [[TConsQ]] */
  type :::[H[_, _[_], _[_], _], T <: TListQ] = TConsQ[H, T]

  /** A type class that witnesses the position of type `T` in type
    * "constructor" list `L`
    */
  trait Pos[L <: TListQ, T[_, _[_], _[_], _]] {
    def index: Int
  }

  object Pos {
    def apply[L <: TListQ, T[_, _[_], _[_], _]](implicit ev: Pos[L, T]): Pos[L, T] = ev
    implicit def materializePos[L <: TListQ, T[_, _[_], _[_], _]]: Pos[L, T] =
      macro internal.TypeListMacros.materializeTListQPos[L, T]
  }

  object Op {
    type Concat [L <: TListQ, R <: TListQ]         <: TListQ
    type Reverse[L <: TListQ]                      <: TListQ
    type Take   [N <: SingletonInt, L <: TListQ]   <: TListQ
    type Drop   [N <: SingletonInt, L <: TListQ]   <: TListQ
    type Remove [K[_, _[_], _[_], _], L <: TListQ] <: TListQ
  }

  trait Compute[L <: TListQ] {
    type Out <: TListQ
  }

  object Compute {
    type Aux[L <: TListQ, O <: TListQ] = Compute[L] { type Out = O }

    def apply[L <: TListQ](implicit ev: Compute[L]): Compute.Aux[L, ev.Out] = ev
    implicit def materializeCompute[L <: TListQ, O <: TListQ]: Aux[L, O] =
      macro internal.TypeListMacros.materializeTListQCompute[L, O]
  }

}
