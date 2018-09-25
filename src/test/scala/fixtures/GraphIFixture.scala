package fixtures

import data._
import scalaz.zio._
import scalaz.{Node ⇒ N, _}
import Scalaz._

/**
  *       2 --- 4 --- 6
  *      /    /
  *    1 --- 3  --- 5
  *
  */
object GraphIFixture {
  private def expensiveComputation(l: Long): Long = {
    l + 1
  }


  val node1 = NodeI[Long, String](1, IO.point("Hello"))
//  val edges: NodeI[Long, String] ⇒ Set[NodeI[Long, String]] = n ⇒  n match {
//    case x if x.id % 2 == 0 ⇒ NodeI()
//    case _ ⇒
//  }

  val node2 = NodeI[Long, String](2, IO.point("there"))

//  val smallGraph = GraphI.build(Set(node1), Set.empty[EdgeI[Int, Int, String]])
}

object GraphFixture {
  object nodes {
    val n1 = Node(1, "H")
    val n2 = Node(2, "e")
    val n3 = Node(3, "o")
    val n4 = Node(4, "l")
    val n5 = Node(5, "t")
    val n6 = Node(6, "l")
  }

  object edges {
    val e12 = Edge(1, 1, 2)
    val e24 = Edge(1, 2, 4)
    val e46 = Edge(1, 4, 6)
    val e13 = Edge(1, 1, 3)
    val e34 = Edge(1, 3, 4)
    val e35 = Edge(1, 3, 5)
  }

  object instances {

    import edges._, nodes._

    val graph: Graph[Int, Int, String] = Graph.create(List(n1, n2, n3, n4, n5, n6), List(e12, e24, e46, e13, e34, e35))

    lazy val medGraph: Graph[Int, Int, Int] = {
      val edges: List[Edge[Int, Int]] = scala.io.Source.fromFile("src/test/resources/med-graph0").getLines().map { line ⇒
        val s :: f :: Nil = line.tail.trim.split(" ").map(_.toInt).toList
        Edge(1, s, f)
      }.toList
      val nodes: List[Node[Int, Int]] = (1 to 500).map(i ⇒ Node(i, i)).toList
      Graph.create(nodes, edges)
    }

    lazy val bigGraph: Graph[Int, Int, String] = Graph.create(
      (1 to 10000).map(i ⇒ Node(i, s"$i")).toList,
      (1 to 9999).toList.flatMap(id ⇒
        List(Edge(id % 3, id, id + 1), Edge(10, id, id + 15))
      )
    )
  }
}
