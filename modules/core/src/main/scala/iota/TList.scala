/* -
 * Iota [iota-core]
 */

package iota

/** A heterogenous list of types */
trait TList

object TList {

  /** A syntactic sugar alias for [[TCons]] */
  type ::[H, T <: TList] = TCons[H, T]

  /** A syntactic sugar alias for [[TCons]] */
  type :::[H, T <: TList] = TCons[H, T]

  /** A type class that witnesses the position of type `A` in type
    * list `L`
    */
  trait Pos[L <: TList, A] {
    def index: Int
  }

  object Pos {
    def apply[L <: TList, A](implicit ev: Pos[L, A]): Pos[L, A] = ev
    implicit def materializePos[L <: TList, A]: Pos[L, A] =
      macro internal.TListMacros.materializePos[L, A]
  }
}
