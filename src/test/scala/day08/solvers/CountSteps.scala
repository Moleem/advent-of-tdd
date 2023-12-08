package day08.solvers

import day08.parsers.MovementMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object CountSteps extends ProblemSolver[MovementMap, Long] {
  override def solve(input: MovementMap): Long =
    input.directions.head match {
      case 'L' if input.mappings("AAA")._1 == "ZZZ" => 1
      case 'R' if input.mappings("AAA")._2 == "ZZZ" => 1
      case _ => ???
    }
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

  it should "find a step count if the target is immediate (right)" in {
    val input = MovementMap(
      directions = List('R'), mappings = Map(
        "AAA" -> ("AAA", "ZZZ")
      )
    )

    CountSteps.solve(input) shouldBe 1
  }

}
