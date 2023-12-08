package day08.solvers

import day08.parsers.MovementMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.annotation.tailrec

object CountSteps extends ProblemSolver[MovementMap, Long] {

  override def solve(input: MovementMap): Long =
    countSteps(input)


  @tailrec
  private def countSteps(
                          input: MovementMap,
                          key: String = "AAA",
                          stepCount: Int = 0
                        ): Int = {
    key match {
      case "ZZZ" => stepCount
      case _ =>
        val directionIndex = stepCount % input.directions.size
        val direction = input.directions(directionIndex)
        val nextKey = direction match {
          case 'L' => input.mappings(key)._1
          case 'R' => input.mappings(key)._2
        }
        countSteps(input, nextKey, stepCount + 1)
    }
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
