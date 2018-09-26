package data

import org.scalatest.{FlatSpec, Matchers}
import fixtures.GraphFixture.instances._
import scalaz._
import scalaz.Scalaz._

class GraphTest extends FlatSpec
  with Matchers
  with Benchmarking {
  import fixtures.GraphFixture.nodes._
  import fixtures.GraphFixture.edges._

  /**
    *       2 --- 4 --- 6
    *      /    /
    *    1 --- 3  --- 5
    *
    */
  behavior of "Construction of the graph"

  it should "guarantee uniqueness of the nodes with the same identifier and content is overridden upon addition" in {
    val updatedGraph = graph +* n1.copy(content = "other")
    updatedGraph.size shouldBe 6
    updatedGraph.node(1) should contain(Node(1, "other"))
  }

  it should "be ok to remove node" in {
    val updatedGraph = graph -* 1
    updatedGraph.size shouldBe 5
    updatedGraph.node(1) shouldBe empty
  }

  it should "be ok to connect nodes" in {
    val updatedGraph = graph *-* Edge(10, 5, 6)
    updatedGraph.size shouldBe 6
    updatedGraph.edgesFrom(5) should contain(Edge(10, 5, 6))
  }

  it should "be ok to disconnect nodes" in {
    val updatedGraph = graph *~* (3, 5)
    updatedGraph.size shouldBe 6
    updatedGraph.edgesFrom(5) shouldBe empty
    updatedGraph.edgesFrom(3) shouldBe Set(e34)
  }


  behavior of "Basic queries on normal graph"

  it should "give all outgoing edges in O(1)" in {
    println(graph)
    graph.edgesFrom(1) shouldBe Set(e12, e13)
  }

  it should "give all neighbors in O(1) for particular node" in {
    graph.neighbors(3) shouldBe Set(4, 5)
  }

  it should "find the edge connecting 2 nodes in O(1)" in {
    graph.edge(1, 6) shouldBe empty
    graph.edge(1, 2) should contain (Edge(1, 1, 2))
  }
  
  behavior of "Algorithms"

  it should "find the shortest path in O(n)" in {
    graph.shortestPath(1, 6).map(_.flatMap(graph.node(_).toIList).map(_.content).fold) shouldBe \/-("Hell")
    graph.shortestPathLength(1, 6) shouldBe \/-(3)
  }

  it should "find the shortest path on medium graph O(n)" in {
    withBenchmark(medGraph.shortestPathLength(500, 1)) shouldBe \/-(2)
  }

  it should "find the shortest path in compact graph fast" in {
    // 33714 ms
    withBenchmark(compact1000.shortestPathLength(1000, 1)) shouldBe \/-(1)
  }

  it should "find the shortest path in sparse graph fast" in {
    // 7000 ms
    withBenchmark(sparse1000.shortestPathLength(1000, 1)) shouldBe \/-(2)
  }

  it should "find the particular node in O(1)" in {
    graph.node(3) should contain (Node(3, "o"))
  }


  it should "give back all disconnected parts" in {
    ???
  }

  it should "return all cycles " in {
    ???
  }



}

trait Benchmarking {
  def withBenchmark[A](f: ⇒ A): A = {
    val t = System.currentTimeMillis()
    val e = f
    println(s"${System.currentTimeMillis() - t} ms")
    e
  }
}
