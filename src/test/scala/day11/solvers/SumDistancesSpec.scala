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
}
