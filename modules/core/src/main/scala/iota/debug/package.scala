package iota  //#=cats
package iotaz //#=scalaz

/** Implicit options to configure/control Iota's macros
  */
package object debug {
  object optionTypes {
    sealed trait ShowTrees
    sealed trait ShowCache
    sealed trait ShowAborts
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

    /** Import this value to have Iota print aborted instance
      * materialization for [[TList]] and [[KList]] helpers
      */
    implicit val ShowAborts: ShowAborts = null.asInstanceOf[ShowAborts]
  }
}
