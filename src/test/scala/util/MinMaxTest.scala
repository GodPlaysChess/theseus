package util

import org.scalatest.{FlatSpec, Matchers}
import scalaz._
import Scalaz._
import MinMax._

class MinMaxTest extends FlatSpec with Matchers {
  "MinMax ordering" should "be correct" in {
    avg(1) < max[Int] shouldBe true
    Order[MinMax[Int]].equal(max[Int],max[Int]) shouldBe true
    Order[MinMax[Int]].equal(min[Int],min[Int]) shouldBe true
    Order[MinMax[Int]].equal(min[Int],max[Int]) shouldBe false
    avg(1) < min[Int] shouldBe false
    avg(1) < avg(2) shouldBe true
    avg(1) < avg(0) shouldBe false

  }

  "MinMax monoid" should "be correct" in {

  }

}
