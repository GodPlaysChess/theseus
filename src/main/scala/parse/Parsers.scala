package parse

import data.{Edge, Graph, Node}
import scalaz.Scalaz._

object Parsers {

  def fromFileEdgesNoLength(path: String): Graph[Int, Int, Int] = {
    val strings = scala.io.Source.fromFile(path).getLines()
    val edges: List[Edge[Int, Int]] = strings.map { line ⇒
      val s :: f :: Nil = line.tail.trim.split(" ").map(_.toInt).toList
      Edge(1, s, f)
    }.toList
    val nodes = (edges.map(_.start).toSet ++ edges.map(_.finish).toSet).map(i ⇒ Node(i, i))
    Graph.create(nodes.toList, edges)
  }

}
