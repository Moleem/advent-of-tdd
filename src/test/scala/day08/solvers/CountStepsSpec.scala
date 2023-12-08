package day08.solvers

import day08.model.MovementMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CountStepsSpec extends AnyFlatSpec with Matchers {

  behavior of CountSteps.getClass.getName

  it should "find step count if the target is immediate (left)" in {
    val input = MovementMap(
      directions = List('L'), mappings = Map(
        "AAA" -> ("ZZZ", "___")
      )
    )

    CountSteps.solve(input) shouldBe 1
  }

  it should "find step count if the target is immediate (right)" in {
    val input = MovementMap(
      directions = List('R'), mappings = Map(
        "AAA" -> ("___", "ZZZ")
      )
    )

    CountSteps.solve(input) shouldBe 1
  }

  it should "find step count if the target is within one loop" in {
    val input = MovementMap(
      directions = List('L', 'L'), mappings = Map(
        "AAA" -> ("BBB", "___"),
        "BBB" -> ("ZZZ", "___")
      )
    )

    CountSteps.solve(input) shouldBe 2
  }

}
