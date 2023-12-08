package day08.solvers

import day08.parsers.MovementMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object CountSteps extends ProblemSolver[MovementMap, Long] {
  override def solve(input: MovementMap): Long = ???
}

class CountStepsSpec extends AnyFlatSpec with Matchers {

  behavior of CountSteps.getClass.getName

  it should "find a step count if the target is immediate (left)" in {
    val input = MovementMap(
      directions = List('L'), mappings = Map(
        "AAA" -> ("ZZZ", "AAA")
      )
    )

    CountSteps.solve(input) shouldBe 1
  }


}
