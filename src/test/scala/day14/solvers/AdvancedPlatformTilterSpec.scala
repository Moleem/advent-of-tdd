package day14.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

class AdvancedPlatformTilter(cycleDirections: List[Char], cycleCount: Int) extends ProblemSolver[String, String] {
  override def solve(input: String): String = ???
}

class AdvancedPlatformTilterSpec extends AnyFlatSpec with Matchers {

  behavior of "AdvancedPlatformTilter"

  it should "be able to tilt north when there are no obstacles" in {
    val input =
      """...
        |.O.
        |...""".stripMargin

    val expectedOutput =
      """.O.
        |...
        |...""".stripMargin

    new AdvancedPlatformTilter(List('N'), 1).solve(input) shouldBe expectedOutput
  }
}
