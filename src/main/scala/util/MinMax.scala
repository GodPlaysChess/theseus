package util

import scalaz.{Monoid, Order, Semigroup}
import scalaz.Ordering.{EQ, GT, LT}
import util.MinMax.{Avg, Max, Min}

/**
  * Enriches any type A with Min and Max element. Which is relevant for the ordering.
  */
sealed abstract class MinMax[A] { self ⇒
  def fold[B](min: ⇒ B, max: ⇒ B, f: A ⇒ B): B = self match {
    case Min() => min
    case Max() => max
    case Avg(value) => f(value)
  }
}

object MinMax {
  private final case class Min[A]() extends MinMax[A]
  private final case class Max[A]() extends MinMax[A]
  private final case class Avg[A](value: A) extends MinMax[A]

  def min[A]: MinMax[A] = Min[A]()
  def max[A]: MinMax[A] = Max[A]()
  def avg[A](a: A): MinMax[A] = Avg(a)

  implicit def minMaxOf[A: Order]: Order[MinMax[A]] = new Order[MinMax[A]] {
    override def order(a1: MinMax[A], a2: MinMax[A]): scalaz.Ordering = (a1, a2) match {
      case (Min(), Min()) ⇒ EQ
      case (Max(), Max()) ⇒ EQ
      case (Min(), _) ⇒ LT
      case (_, Max()) ⇒ LT
      case (Max(), _) ⇒ GT
      case (_, Min()) ⇒ GT
      case (Avg(v1), Avg(v2)) ⇒ Order[A].order(v1, v2)
    }
  }

  implicit def monoid[A: Semigroup]: Monoid[MinMax[A]] = new Monoid[MinMax[A]] {
    override def zero: MinMax[A] = min[A]

    override def append(f1: MinMax[A], f2: ⇒ MinMax[A]): MinMax[A] = (f1, f2) match {
      case (Min(), _) ⇒ f2
      case (Max(), _) ⇒ f1
      case (Avg(a), Avg(b)) ⇒ Avg(Semigroup[A].append(a, b))
      case (_, _) ⇒ append(f2, f1)
    }
  }

}
