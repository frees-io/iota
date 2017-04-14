/* -
 * Iota [iota-core]
 */

package object iota {

  /** A heterogenous list of type constructors */
  type KList

  /** The terminal element of a type constructor list */
  type KNil <: KList

  /** A type constructor list characterized by a head type
    * constructor and a list of tail type constructors
    *
    * @tparam H the head type constructor
    * @tparam T the list of tail type constructors
    */
  type KCons[H[_], T <: KList] <: KList

  /** A heterogenous list of types */
  type TList

  /** The terminal element of a type list */
  type TNil <: TList

  /** A type list characterized by a head type and a list of tail types
    *
    * @tparam H the head type
    * @tparam T the list of tail types
    */
  type TCons[H, T <: TList] <: TList

}
