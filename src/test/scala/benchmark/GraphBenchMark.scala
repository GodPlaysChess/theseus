//package benchmark
//
//import data.Graph
//import org.scalameter.api._
//import scalaz._
//import Scalaz._
//import org.scalameter.picklers.Pickler
//
//class GraphBenchMark extends Bench.LocalTime {
//
//  implicit val graphPickler: Pickler[Graph[Int, Int, Int]] = Pickler.makeInstance(classOf[Graph[Int, Int, Int]])
//  val graphGen: Gen[Graph[Int, Int, Int]] = Gen.single("graph gen")(fixtures.GraphFixture.instances.medGraph)
//
//  performance of "graph" in {
//    measure method "shortest path" in {
//       using[Graph[Int, Int, Int]](graphGen) in { g ⇒
//         g.shortestPath(1, 1000)
//       }
//    }
//  }
//}
