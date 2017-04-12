/* -
 * Iota [iota-core]
 */

package object iota {
  type KList
  type KNil <: KList
  type KCons[H[_], T <: KList] <: KList

  type TList
  type TNil <: TList
  type TCons[H, T <: TList] <: TList
}
