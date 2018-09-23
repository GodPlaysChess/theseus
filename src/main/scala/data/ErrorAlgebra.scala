package data

import scalaz.Show

sealed abstract class ErrorAlgebra
object ErrorAlgebra {
  private case object GenericError extends ErrorAlgebra
  private case object TraverseError extends ErrorAlgebra
  private case object Cycle extends ErrorAlgebra
  private case object NoSuchNode extends ErrorAlgebra
  private case object NodeUnreachable extends ErrorAlgebra

  def genericError: ErrorAlgebra = GenericError
  def traverseError: ErrorAlgebra = TraverseError
  def cycle: ErrorAlgebra = Cycle
  def noSuchNode: ErrorAlgebra = NoSuchNode
  def nodeUnreachable: ErrorAlgebra = NodeUnreachable

  case class NodeEvaluationProblem[I: Show](nodeId: I)


}

