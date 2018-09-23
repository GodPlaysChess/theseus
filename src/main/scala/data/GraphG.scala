package data

import scalaz.{Equal, ISet}
import scalaz.zio.IO
import scalaz._
import Scalaz._


/**
  * Graph with a known geometry, but with unknown content
  */
class GraphG[I: Equal, L, A](private val nodes: Set[NodeI[I, A]], private val edges: Set[Edge[I, L]]) {

  /**
    * Evaluating all node content in the graph. Since geometry is already known - no need to traverse it upon evaluation.
    * Though sometimes for big graphs, we might need the result of the neighbouring nodes earlier.
    */
  def evaluate(start: I): IO[ErrorAlgebra, Graph[I, L, A]] = {
//    IO.parAll(nodes.map(_.evaluate)) map { ns: List[Node[I, A]] ⇒
//      new Graph(ns.toSet, edges)
//    }
    ???
  }


  /**
    * Looks for the element in the graph
    */
  def find(i: I): Option[NodeI[I, A]] = ???

  def map[B](f: A ⇒ B): GraphG[I, L, B] = {
    new GraphG[I, L, B](nodes.map(_.map(f)), edges)
  }

}





