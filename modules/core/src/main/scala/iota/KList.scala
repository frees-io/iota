/* -
 * Iota [iota-core]
 */

package iota

/** A heterogenous list of type constructors */
trait KList

object KList {

  /** A syntactic sugar alias for [[KCons]] */
  type ::[H[_], T <: KList] = KCons[H, T]

  /** A syntactic sugar alias for [[KCons]] */
  type :::[H[_], T <: KList] = KCons[H, T]

  /** A type class that witnesses the position of type constructor `F` in type
    * constructor list `L`
    */
  trait Pos[L <: KList, F[_]] {
    def index: Int
  }

  object Pos {
    def apply[L <: KList, F[_]](implicit ev: Pos[L, F]): Pos[L, F] = ev
    implicit def materializePos[L <: KList, F[_]]: Pos[L, F] =
      macro internal.KListMacros.materializePos[L, F]
  }
}
