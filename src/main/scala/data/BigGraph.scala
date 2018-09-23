package data


/**
  * This could be a zipper, which is operating on the small graph
  */
object BigGraph {
  case class BNode[I, A, L](i: I, a: A, out: List[BEdge[I, A, L]])
  case class BEdge[I, A, L](s: BNode[I, A, L], f: BNode[I, A, L], l: L)
}
