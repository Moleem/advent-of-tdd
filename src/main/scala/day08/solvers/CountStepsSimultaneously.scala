package day08.solvers

import day08.model.MovementMap
import utils.ProblemSolver

import scala.annotation.tailrec

object CountStepsSimultaneously extends ProblemSolver[MovementMap, Long] {

  @tailrec
  private def getNextZValueAndDistance(input: MovementMap, key: String, stepCount: Int = 1): (String, Int) = {
    val directionIndex = (stepCount-1) % input.directions.size
    val direction = input.directions(directionIndex)
    val nextKey =
      direction match {
        case 'L' => input.mappings(key)._1
        case 'R' => input.mappings(key)._2
      }

    if (nextKey.endsWith("Z"))
      (nextKey, stepCount)
    else {
      getNextZValueAndDistance(input, nextKey, stepCount + 1)
    }
  }


  override def solve(input: MovementMap): Long = {
    val distanceMetrics = input
      .mappings
      .keySet
      .filter(_.endsWith("A"))
      .map{ key =>
        val (nextX, initialDistanceToX) = getNextZValueAndDistance(input, key)
        val (lastX, repeatingLength) = getNextZValueAndDistance(input, nextX)
        (initialDistanceToX, repeatingLength)
      }.toList

    val startDistance = distanceMetrics.head._1
    val repeatDistance = distanceMetrics.head._2

    @tailrec
    def helper(current: Long): Long = {
      if (distanceMetrics.forall(m => (current-m._1)%m._2 == 0))
        current
      else
        helper(current + repeatDistance)
    }


    helper(startDistance)

  }

}
