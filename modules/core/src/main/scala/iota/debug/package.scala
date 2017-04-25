/* -
 * Iota [iota-core]
 */

package iota

/** Implicit options to configure/control Iota's macros
  */
package object debug {
  object optionTypes {
    sealed trait ShowTrees
    sealed trait ShowCache
  }
  import optionTypes._

  object options {
    /** Import this value to have Iota print the macro generated code
      * to the console during compilation
      */
    implicit val ShowTrees: ShowTrees = null.asInstanceOf[ShowTrees]

    /** Import this value to have Iota print the cached computations
      * during macro expansion
      */
    implicit val ShowCache: ShowCache = null.asInstanceOf[ShowCache]
  }
}
