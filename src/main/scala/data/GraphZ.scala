package data

import scalaz.Scalaz._
import scalaz.{==>>, _}
import util.MinMax

import scala.annotation.tailrec

class GraphZ[I: Order, L, A] private(private val nodes: I ==>> Node[I, A], private val edges: I ==>> NonEmptyList[Edge[I, L]]) { self ⇒
  implicit val orderEdge: Order[Edge[I, L]] = Order.orderBy(_.start)

  def size = nodes.size

  def allNodes: List[Node[I, A]] = nodes.values

  def allEdges: List[Edge[I, L]] = edges.values.flatMap(_.toList)

  /**
    * Looks up certain node
    */
  def node(id: I): Option[Node[I, A]] = nodes.lookup(id)

  /**
    * Looks up edge, connecting two nodes.
    */
  def edge(s: I, f: I): Option[Edge[I, L]] = edgesFrom(s).find(_.finish === f)

  /**
    * Looks up all edges, starting in node
    */
  def edgesFrom(node: I): IList[Edge[I, L]] = edges.lookup(node).toIList.flatMap(_.toIList)

  /**
    * all neighbors for the node
    */
  def neighbors(node: I): IList[I] = edgesFrom(node).map(_.finish)


  /**
    * Adds node to the graph.
    */
  def +*(n: Node[I, A]): GraphZ[I, L, A] = {
    new GraphZ[I, L, A](nodes + (n.id → n), edges)
  }

  /**
    * Removes node to the graph.
    */
  def -*(n: I): GraphZ[I, L, A] = {
    ???
//    new GraphZ[I, L, A](nodes - n, (edges - n).map(_.filter(_.finish != n)))
  }

  /**
    * Adds nodes to the graph.
    */
  def ++*[F[_]: Foldable](moreNodes: F[Node[I, A]]): GraphZ[I, L, A] = {
    new GraphZ[I, L, A](Foldable[F].foldl(moreNodes, nodes)(ns ⇒ n ⇒ ns + (n.id → n)), edges)
  }

  def *-* : Edge[I, L] ⇒ GraphZ[I, L, A] = { connectE(_) | self }

  /**
    * None if either f or s does not exists. Alternatively can return Node(S) does not exist message
    * */
  def connectE(e: Edge[I, L]): ErrorAlgebra \/ GraphZ[I, L, A] = e match { case Edge(l, s, f) ⇒
    (nodes.member(s) && nodes.member(f)).either(
      new GraphZ(nodes, edges.insertWith((a, b) ⇒ a |+| b, s, NonEmptyList(Edge(l, s, f))))
    ) or ErrorAlgebra.noSuchNode
  }

  def *~* : (I, I) ⇒ GraphZ[I, L, A] = disconnect

  def disconnect(s: I, f: I): GraphZ[I, L, A] = {
    edges.lookup(s).flatMap(_.toIList.filter(_.finish =/= f).toNel).cata( e2 ⇒
      new GraphZ(nodes, edges + (s -> e2)),
      new GraphZ(nodes, edges - s)
    )
  }

  def shortestPathLength(s: I, f: I)(implicit S: Monoid[L], OL: Order[L]): ErrorAlgebra \/ L = {
    ((nodes.member(s) && nodes.member(f)).either(
      distances(s)._1.lookup(f).get.fold(S.zero.right[ErrorAlgebra], ErrorAlgebra.nodeUnreachable.left, _.right)
    ) or ErrorAlgebra.noSuchNode).join
  }

  /* TODO use a Fibonacci heap, as it should be more perfomant */
  def shortestPath(s: I, f: I)(implicit S: Semigroup[L], OL: Order[L]): ErrorAlgebra \/ IList[I] = {
    (nodes.member(s) && nodes.member(f)).either(
      (f ##:: EphemeralStream.unfold(f)(distances(s)._2.lookup(_).join.fpair)).reverse.toIList
    ) or ErrorAlgebra.noSuchNode
  }

  private def distances(s: I)(implicit S: Semigroup[L], OL: Order[L]): (I ==>> MinMax[L], I ==>> Option[I]) = {
    import util.MinMax._
    implicit val O: Order[(I, MinMax[L])] = minMaxOf[L].contramap(_._2)

    val dist: I ==>> MinMax[L] = nodes.map(_ ⇒ max[L]) + (s → min[L])
    val queue: Heap[(I, MinMax[L])] = Heap.fromData(dist.fold(IList.empty[(I, MinMax[L])])((k, v, l) ⇒ (k, v) :: l))
    val prevs = nodes.map(_ ⇒ none[I])

    @tailrec
    def go(heap: Heap[(I, MinMax[L])], dist: I ==>> MinMax[L], prev: I ==>> Option[I]): (I ==>> MinMax[L], I ==>> Option[I]) = {
      heap.uncons match {
        case None => dist → prev
        case Some((min, remheap)) =>
          val (dist1, prev1, remheap1) = edgesFrom(min._1).foldLeft((dist, prev, remheap)) { case ((dst, prv, rem), out) ⇒
            val alt: Option[MinMax[L]] = dst.lookup(out.start) |+| avg(out.length).some // coherence between edges and nodes is ensured upon construction
            if (alt < dst.lookup(out.finish)) {
              (dst.update(out.finish, _ ⇒ alt), prv.update(out.finish, _ ⇒ Option(Option(out.start))), rem.map(x ⇒
                alt.filter(_ ⇒ x._1 === out.finish).map(x >| _) | x
//                if (x._1 === out.finish) x >| alt.get else x // todo could possibly optimize that operation, by mapping just once on each recursion step. Just fold within the map.
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

object GraphZ {
  // todo change to foldable
  def create[I: Order, A, L](nodes: List[Node[I, A]], edges: List[Edge[I, L]]) = {
//    val filtr = { e: Edge[I, L] ⇒ (nodes.map(_.id).contains(_)).product{(e.start, e.finish)}.fold(booleanInstance.conjunction.append) }

    new GraphZ[I, L, A](==>>.fromList(nodes.fproduct(_.id).map(_.swap)), edges.toIList
      .filter(e ⇒ nodes.map(_.id).contains(e.finish) && nodes.map(_.id).contains(e.start))
      .groupBy(_.start))
  }

  def createdUndirected[I: Equal, A, L](nodes: List[Node[I, A]], edges: List[Edge[I, L]]) = {
    ???
  }

  // that does not make any sence to mix scalaz and native concepts there.
  // But OTOH it'll go away as soon as I move to scalaz datastructures entirely
  implicit def graphInstance[I, A, L]: Equal[GraphZ[I, A, L]] = new Equal[GraphZ[I, A, L]] {
    override def equal(a1: GraphZ[I, A, L], a2: GraphZ[I, A, L]): Boolean = {
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