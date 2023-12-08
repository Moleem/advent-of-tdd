package day08.solvers

import day08.model.MovementMap
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
