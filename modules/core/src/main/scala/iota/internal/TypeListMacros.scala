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

import scala.reflect.macros.blackbox.Context

private[iota] final class TypeListMacros(
  cc: Context
) extends IotaMacroToolbelt(cc) {
  import c.universe._

  def materializeTListPos[L <: TList, A](
    implicit
      evL: c.WeakTypeTag[L],
      evA: c.WeakTypeTag[A]
  ): c.Expr[TList.Pos[L, A]] = {

    val L = evL.tpe.dealias
    val A = evA.tpe.dealias

    foldAbort(for {
      algebras <- memoizedTListTypes(L)
      index    <- Right(algebras.indexWhere(_ =:= A))
                    .filterOrElse(_ >= 0, s"$A is not a member of $L")
    } yield
      q"new _root_.iota.TList.Pos[$L, $A]{ override val index: Int = $index }", true)
  }

  def materializeKListPos[L <: KList, F[_]](
    implicit
      evL: c.WeakTypeTag[L],
      evF: c.WeakTypeTag[F[_]]
  ): c.Expr[KList.Pos[L, F]] = {

    val L = evL.tpe.dealias
    val F = evF.tpe.dealias

    foldAbort(for {
      algebras <- memoizedKListTypes(L)
      index    <- Right(algebras.indexWhere(_ =:= F))
                    .filterOrElse(_ >= 0, s"$F is not a member of $L")
    } yield
      q"new _root_.iota.KList.Pos[$L, $F]{ override val index: Int = $index }", true)
  }

}
