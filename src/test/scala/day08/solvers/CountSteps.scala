package day08.solvers

import day08.parsers.MovementMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object CountSteps extends ProblemSolver[MovementMap, Long] {
  override def solve(input: MovementMap): Long = {
    var current = "AAA"
    var stepCount = 0

    while(current != "ZZZ") {
      val directionIndex = stepCount % input.directions.size
      current = getNext(input.mappings, current, input.directions(directionIndex))
      stepCount += 1
    }

    stepCount
  }


  private def getNext(mappings: Map[String, (String, String)], key: String, direction: Char): String =
    direction match {
      case 'L' => mappings(key)._1
      case 'R' => mappings(key)._2
    }
}

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
