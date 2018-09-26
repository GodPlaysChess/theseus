package data

import scalaz.Scalaz._
import scalaz._
import util.MinMax

import scala.annotation.tailrec

/**
  * @tparam I - identifier
  * @tparam L - length of the edge. // one by default, but can be request time in ms, or physical length
  */
case class Edge[I, L](length: L, start: I, finish: I)

/**
  * Pick one of the following to represent the Node:
  */
case class Node[I, A](id: I, content: A)

// Equal I does not need until we use such constraint in the map
// For directed graph, for every edge  there's another edge with swapped direction. Or UndirectedEdg
class Graph[I: Equal, L, A] private(private val nodes: Map[I, Node[I, A]], private val edges: Map[I, Set[Edge[I, L]]]) { self ⇒

  def size = nodes.size

  def allNodes: Set[Node[I, A]] = nodes.values.toSet

  def allEdges: Set[Edge[I, L]] = edges.values.toSet.flatten

  /**
    * Looks up certain node
    */
  def node(id: I): Option[Node[I, A]] = nodes.get(id)

  /**
    * Looks up edge, connecting two nodes.
    */
  def edge(s: I, f: I): Option[Edge[I, L]] = edgesFrom(s).find(_.finish === f)

  /**
    * Looks up all edges, starting in node
    */
  def edgesFrom(node: I): Set[Edge[I, L]] = edges.getOrElse(node, Set.empty)

  /**
    * all neighbors for the node
    */
  def neighbors(node: I): Set[I] = edgesFrom(node).map(_.finish)


  /**
    * Adds node to the graph.
    */
  def +*(n: Node[I, A]): Graph[I, L, A] = {
    new Graph[I, L, A](nodes + (n.id → n), edges)
  }

  /**
    * Removes node to the graph.
    */
  def -*(n: I): Graph[I, L, A] = {
    new Graph[I, L, A](nodes - n, (edges - n).mapValues(_.filter(_.finish != n)))
  }

  /**
    * Adds nodes to the graph.
    */
  def ++*[F[_]: Foldable](moreNodes: F[Node[I, A]]): Graph[I, L, A] = {
    new Graph[I, L, A](Foldable[F].foldl(moreNodes, nodes)(ns ⇒ n ⇒ ns + (n.id → n)), edges)
  }

  def *-* : Edge[I, L] ⇒ Graph[I, L, A] = connectE(_) getOrElse self

  /**
    * None if either f or s does not exists. Alternatively can return Node(S) does not exist message
    * */
  def connectE(e: Edge[I, L]): ErrorAlgebra \/ Graph[I, L, A] = e match { case Edge(l, s, f) ⇒
    (nodes.contains(s) && nodes.contains(f)).either(
      new Graph(nodes, edges.insertWith(s, Set(Edge(l, s, f)))(_ ++ _))
    ) or ErrorAlgebra.noSuchNode
  }

  def *~* : (I, I) ⇒ Graph[I, L, A] = disconnect

  def disconnect(s: I, f: I): Graph[I, L, A] = {
    edges.get(s).cata(edgesForS ⇒ {
      val es2 = edgesForS.filter(_.finish != f)
      val edges1 = if (es2.isEmpty) edges - s else edges + (s -> es2)
      new Graph(nodes, edges1)
    }, self)
  }

  def shortestPathLength(s: I, f: I)(implicit S: Monoid[L], OL: Order[L]): ErrorAlgebra \/ L = {
    ((nodes.contains(s) && nodes.contains(f)).either(
      distances(s)._1(f).fold(S.zero.right[ErrorAlgebra], ErrorAlgebra.nodeUnreachable.left, _.right)
    ) or ErrorAlgebra.noSuchNode).join
  }

  /* TODO use a Fibonacci heap, as it should be more perfomant */
  def shortestPath(s: I, f: I)(implicit S: Semigroup[L], OL: Order[L]): ErrorAlgebra \/ IList[I] = {
    (nodes.contains(s) && nodes.contains(f)).either(
      (f ##:: EphemeralStream.unfold(f)(distances(s)._2.get(_).join.fpair)).reverse.toIList
    ) or ErrorAlgebra.noSuchNode
  }

  private def distances(s: I)(implicit S: Semigroup[L], OL: Order[L]): (Map[I, MinMax[L]], Map[I, Option[I]]) = {
    import util.MinMax._
    implicit val O: Order[(I, MinMax[L])] = minMaxOf[L].contramap(_._2)

    val dist: Map[I, MinMax[L]] = nodes.mapValues(_ ⇒ max[L]) + (s → min[L])
    val queue: Heap[(I, MinMax[L])] = Heap.fromData(dist.toSet.toIList)
    val prevs = nodes.mapValues(_ ⇒ none[I])

    @tailrec
    def go(heap: Heap[(I, MinMax[L])], dist: Map[I, MinMax[L]], prev: Map[I, Option[I]]): (Map[I, MinMax[L]], Map[I, Option[I]]) = {
      heap.uncons match {
        case None => dist → prev
        case Some((min, remheap)) =>
          val (dist1, prev1, remheap1) = edgesFrom(min._1).foldLeft((dist, prev, remheap)) { case ((dst, prv, rem), out) ⇒
            val alt = dst(out.start) |+| avg(out.length) // coherence between edges and nodes is ensured upon construction
            if (alt < dst(out.finish)) {
              (dst.updated(out.finish, alt), prv.updated(out.finish, out.start.some), rem.map(x ⇒
                if (x._1 === out.finish) x >| alt else x // todo could possibly optimize that operation, by mapping just once on each recursion step. Just fold within the map.
              ))
            }
            else (dst, prv, rem)
          }

          go(remheap1, dist1, prev1)
      }
    }

    go(queue, dist, prevs)
  }

  /**
    * @return all cycles within the graph
    */
  def cycles: Set[IList[I]] = ???
}

object Graph {
  // todo change Equal to Order constraint to use scalaz datatypes
  def create[I: Equal, A, L](nodes: List[Node[I, A]], edges: List[Edge[I, L]]) = {
//    val filtr = { e: Edge[I, L] ⇒ (nodes.map(_.id).contains(_)).product{(e.start, e.finish)}.fold(booleanInstance.conjunction.append) }

    new Graph[I, L, A](nodes.fproduct(_.id).map(_.swap).toMap, edges
      .filter(e ⇒ nodes.map(_.id).contains(e.finish) && nodes.map(_.id).contains(e.start))
      .groupBy(_.start).mapValues(_.toSet))
  }

  def createdUndirected[I: Equal, A, L](nodes: List[Node[I, A]], edges: List[Edge[I, L]]) = {
    ???
  }

  // that does not make any sence to mix scalaz and native concepts there.
  // But OTOH it'll go away as soon as I move to scalaz datastructures entirely
  implicit def graphInstance[I, A, L]: Equal[Graph[I, A, L]] = new Equal[Graph[I, A, L]] {
    override def equal(a1: Graph[I, A, L], a2: Graph[I, A, L]): Boolean = {
      a1.allNodes == a2.allNodes && a1.allEdges == a2.allEdges
    }
  }
}



/**
  *
  * /**
  * * Alternative way of encoding node and graph:
  * * Actually this one is more correct, since out - should be constant.
  **/
  * case class NodeE[I: Equal, A, L](id: I, content: A, out: Set[Edge[I, L]])
  *
  * */