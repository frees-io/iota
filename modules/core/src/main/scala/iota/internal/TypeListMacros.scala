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

private[iota] final class TypeListMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def materializeTListPos[L <: TList, A](
    implicit
      evL: c.WeakTypeTag[L],
      evA: c.WeakTypeTag[A]
  ): c.Expr[TList.Pos[L, A]] = {

    val L = evL.tpe
    val A = evA.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L)
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

    val L = evL.tpe
    val F = evF.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedKListTypes(L)
      index    <- Right(algebras.indexWhere(_ =:= F))
                    .filterOrElse(_ >= 0, s"$F is not a member of $L")
    } yield
      q"new _root_.iota.KList.Pos[$L, $F]{ override val index: Int = $index }", true)
  }

  def materializeTListCompute[L <: TList, O <: TList](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[TList.Compute.Aux[L, O]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L)
      tpe       = tb.buildTList(algebras)
    } yield
      q"new _root_.iota.TList.Compute[$L]{ override type Out = $tpe }", true)
  }

  def materializeKListCompute[L <: KList, O <: KList](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[KList.Compute.Aux[L, O]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedKListTypes(L)
      tpe       = tb.buildKList(algebras)
    } yield
      // q"new _root_.iota.KList.Compute[$L]{ override type Out = $tpe }", true)
      q"""new _root_.iota.KList.Compute[$L]{
            override type Out = $tpe
            def apply = new _root_.cats.arrow.FunctionK[
              ({ type λ[α] = _root_.iota.CopK[$L, α] })#λ,
              ({ type λ[α] = _root_.iota.CopK[$tpe, α] })#λ
            ] {
              def apply[ZZ](a: _root_.iota.CopK[$L, ZZ]) =
                a.asInstanceOf[CopK[$tpe, ZZ]]
            }
          }""", true)
  }

}
