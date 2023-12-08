package day08.solvers

import day08.model.MovementMap
import utils.ProblemSolver

import scala.annotation.tailrec

object CountStepsSimultaneously extends ProblemSolver[MovementMap, Int] {
  override def solve(input: MovementMap): Int =
    countSteps(input, input.mappings.keySet.filter(_.endsWith("A")))

  @tailrec
  private def countSteps(
                          input: MovementMap,
                          keys: Set[String],
                          stepCount: Int = 0,
                          seenInputs: Set[Set[String]] = Set.empty
                        ): Int = {
    if (seenInputs.contains(keys)) throw new RuntimeException
    else if (keys.forall(_.endsWith("Z"))) stepCount
    else {
      if (stepCount % 1000 == 0) {
        println(stepCount)
        println(keys)
      }
      val directionIndex = stepCount % input.directions.size
      val direction = input.directions(directionIndex)
      val nextKeys = keys.map(key =>
        direction match {
          case 'L' => input.mappings(key)._1
          case 'R' => input.mappings(key)._2
        })
      countSteps(input, nextKeys, stepCount + 1, (keys :: seenInputs.toList).toSet)
    }
  }
}
