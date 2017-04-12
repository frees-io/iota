/* -
 * Iota [iota-core]
 */

package object iotatests {
  // needed by Scalacheck when no predef is used
  private[iotatests] implicit def identityView[A](a: A): A = a
}
