package day11.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SumDistancesSpec extends AnyFlatSpec with Matchers {

  behavior of SumDistances.getClass.getName

  it should "correctly get distance horizontally" in {
    val input = Map(
      0 -> (0, 0),
      1 -> (0, 5)
    )

    SumDistances.solve(input) shouldBe 5
  }

  it should "correctly get distance vertically" in {
    val input = Map(
      0 -> (0, 0),
      1 -> (5, 0)
    )

    SumDistances.solve(input) shouldBe 5
  }

  it should "correctly get distance diagonally" in {
    val input = Map(
      0 -> (0, 0),
      1 -> (5, 5)
    )

    SumDistances.solve(input) shouldBe 10
  }

  it should "correctly get distance diagonally (other way)" in {
    val input = Map(
      0 -> (0, 5),
      1 -> (5, 0)
    )

    SumDistances.solve(input) shouldBe 10
  }

  it should "correctly get sum of multiple distances" in {
    val input = Map(
      0 -> (0, 0), // 0-1 = 5, 0-2 = 5, 0-3 = 10
      1 -> (0, 5), // 1-2 = 10, 1-3 = 5
      2 -> (5, 0), // 2-3 = 5
      3 -> (5, 5)
    )

    SumDistances.solve(input) shouldBe 40 // 5 + 5 + 10 + 10 + 5 + 5
  }
}
