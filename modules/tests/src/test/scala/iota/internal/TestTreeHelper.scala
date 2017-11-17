//#+jvm

package iota  //#=cats
package iotaz //#=scalaz
package internal

import catryoshka._

class TestTreeHelper(val tb: Toolbelt with TypeListAST) {
  import tb._
  import tb.u.{ Type, WeakTypeTag }

  def t[T](implicit evT: WeakTypeTag[T]): Type = evT.tpe

  type Node = Fix[NodeF]

  val nnil                          : Fix[NodeF] = Fix[NodeF](NNilF)
  def cons[T](node: Fix[NodeF] = nnil)(
    implicit evT: WeakTypeTag[T])   : Fix[NodeF] = Fix[NodeF](ConsF(evT.tpe, node))
  def consk[T[_]](node: Fix[NodeF] = nnil)(
    implicit evT: WeakTypeTag[T[_]]): Fix[NodeF] = Fix[NodeF](ConsF(evT.tpe.typeConstructor, node))
  def concat(nodes: Fix[NodeF]*)    : Fix[NodeF] = Fix[NodeF](ConcatF(nodes.toList))
  def reverse(node: Fix[NodeF])     : Fix[NodeF] = Fix[NodeF](ReverseF(node))
  def take(n: Int, node: Fix[NodeF]): Fix[NodeF] = Fix[NodeF](TakeF(n, node))
  def drop(n: Int, node: Fix[NodeF]): Fix[NodeF] = Fix[NodeF](DropF(n, node))
  def remove[T](node: Fix[NodeF] = nnil)(
    implicit evT: WeakTypeTag[T])   : Fix[NodeF] = Fix[NodeF](RemoveF(evT.tpe, node))
  def removek[T[_]](node: Fix[NodeF] = nnil)(
    implicit evT: WeakTypeTag[T[_]]): Fix[NodeF] = Fix[NodeF](RemoveF(evT.tpe.typeConstructor, node))
}

//#-jvm
