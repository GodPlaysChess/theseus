package data

import scalaz.Equal
import scalaz.zio.IO

/**
  * @tparam I - identifier (something with fast hashcode)
  * @tparam L - length of the edge. // one by default, but can be request time in ms, or physical length
  * @tparam A - content of the Node
  */
case class EdgeI[I: Equal, L, A](length: L, run: I ⇒ IO[ErrorAlgebra, NodeI[I, A]]) {
  def map[B](f: A ⇒ B): EdgeI[I,L,B] = EdgeI(length, i ⇒ run(i).map(_.map(f)))
}

/**
  * content of the node might take time to evaluate.
  */
case class NodeI[I: Equal, A](id: I, content: IO[ErrorAlgebra, A]) {
  def map[B](f: A ⇒ B): NodeI[I, B] = NodeI(id, content.map(f))

  def evaluate: IO[ErrorAlgebra, Node[I, A]] = {
    content.map(a ⇒ Node(id, a))
  }
}
