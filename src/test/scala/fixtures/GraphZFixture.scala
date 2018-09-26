package fixtures

import data._
import parse.Parsers
import scalaz.Scalaz._

/**
  *       2 --- 4 --- 6
  *      /    /
  *    1 --- 3  --- 5
  *
  */
object GraphZFixture {

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
    import edges._
    import nodes._

    val graph: GraphZ[Int, Int, String] = GraphZ.create(List(n1, n2, n3, n4, n5, n6), List(e12, e24, e46, e13, e34, e35))

    lazy val medGraphZ: GraphZ[Int, Int, Int] = Parsers.fromFileEdgesNoLengthZ("src/test/resources/med-graph0")
    lazy val compact1000: GraphZ[Int, Int, Int] = Parsers.fromFileEdgesNoLengthZ("src/test/resources/1000-compact")
    lazy val sparse1000: GraphZ[Int, Int, Int] = Parsers.fromFileEdgesNoLengthZ("src/test/resources/1000-sparse")

    lazy val bigGraphZ: GraphZ[Int, Int, String] = GraphZ.create(
      (1 to 10000).map(i ⇒ Node(i, s"$i")).toList,
      (1 to 9999).toList.flatMap(id ⇒
        List(Edge(id % 3, id, id + 1), Edge(10, id, id + 15))
      )
    )
  }
}
