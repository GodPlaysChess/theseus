package data

import scalaz.Equal
import scalaz.zio.IO


/**
  * Such graph supports all of the following cases:
  * known nodes, with not-yet evaluated content
  * unexplored nodes, which come from the edges evaluation.
  */
class GraphI[I: Equal, L, A](nodes: Set[NodeI[I, A]], edges: Set[EdgeI[I, L, A]]) {

  /**
    * walks through the graph,  starting in the node I and evaluates all the IOs on the way.
    * It chooses between breadth first or depth first, based on empiric information.
    * Can be faster if some information about graph is provided in advance (Sparse, Compact) graph.
    * After unsafe perform IO - getting rid of IO completely.
    */
  def evaluate(start: I): IO[ErrorAlgebra, Graph[I, L, A]] = {
    def step(visited: Set[I], graph: Graph[I, L, A], remaining: Set[I]): IO[ErrorAlgebra, Graph[I, L, A]] = {
        ???
    }
    ???
//    step(Set.empty, new Graph[I,L,A](Set.empty, Set.empty), Set.empty)
  }

  /**
    * Explores the graph geometry without evaluating its content
    */
  def explore: GraphG[I, L, A] = {
    ???
  }

  /**
    * Looks for the element in the graph
    */
  def find(i: I): Option[NodeI[I, A]] = ???

  def map[B](f: A ⇒ B): GraphI[I, L, B] = {
    new GraphI[I, L, B](nodes.map(_.map(f)), edges.map(_.map(f)))
  }

}

/**
  * Graph build and graph ops should be separate.
  * */

object GraphI {
  def build[I: Equal, L, A](nodes: Set[NodeI[I, A]], edges: Set[EdgeI[I, L, A]]): GraphI[I, L, A] = {
    new GraphI[I, L, A](nodes, edges)
  }
}



