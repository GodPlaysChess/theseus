package data

import org.scalatest.{FlatSpec, Matchers}
import fixtures.GraphFixture.instances._
import scalaz._
import scalaz.Scalaz._

class GraphTest extends FlatSpec with Matchers {
  import fixtures.GraphFixture.nodes._
  import fixtures.GraphFixture.edges._

  behavior of "Basic operations on normal graph"


  /**
    *       2 --- 4 --- 6
    *      /    /
    *    1 --- 3  --- 5
    *
    */
  it should "guarantee uniqueness of the nodes with the same identifier and content is overridden upon addition" in {
    val updatedGraph = graph +* n1.copy(content = "other")
    updatedGraph.size shouldBe 6
    updatedGraph.node(1) should contain(Node(1, "other"))


  }

  it should "give all outgoing edges in O(1)" in {
    graph.allEdges(1) shouldBe Set(e12, e13)
  }

  it should "give all neighbors in O(1) for particular node" in {
    graph.neighbors(3) shouldBe Set(4, 5)
  }

  it should "find the shortest path in O(n)" in {
    graph.shortestPath(1, 6).flatMap(graph.node(_).toIList).map(_.content).fold shouldBe "Hell"
    graph.shortestPathLength(1, 6) shouldBe \/-(3)
  }

  it should "find the particular node in O(1)" in {
    graph.node(3) should contain (Node(3, "o"))
  }

  it should "find the edge connecting 2 nodes in O(1)" in {
    graph.edge(1, 6) shouldBe empty
    graph.edge(1, 2) should contain (Edge(1, 1, 2))
  }

  it should "give back all disconnected parts" in {
    ???
  }

  it should "return all cycles in " {
    ???
  }



}
