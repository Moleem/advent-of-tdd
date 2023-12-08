package day08.solvers

import day08.model.MovementMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CountStepsSimultaneouslySpec extends AnyFlatSpec with Matchers {

  behavior of CountStepsSimultaneously.getClass.getName

  it should "find step count if the target is immediate (left)" in {
    val input = MovementMap(
      directions = List('L'), mappings = Map(
        "11A" -> ("11Z", "___"),
        "22A" -> ("22Z", "___")
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 1
  }

  it should "find step count if the target is immediate (right)" in {
    val input = MovementMap(
      directions = List('R'), mappings = Map(
        "11A" -> ("___", "11Z"),
        "22A" -> ("___", "22Z")
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 1
  }

  it should "find step count if the target is within one loop" in {
    val input = MovementMap(
      directions = List('L', 'L'), mappings = Map(
        "11A" -> ("11B", "___"),
        "22A" -> ("22B", "___"),
        "11B" -> ("11Z", "___"),
        "22B" -> ("22Z", "___"),
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 2
  }

}
