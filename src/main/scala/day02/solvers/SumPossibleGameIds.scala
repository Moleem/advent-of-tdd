package day02.solvers

import day02.model.GameRecord
import utils.ProblemSolver

object SumPossibleGameIds extends ProblemSolver[Seq[GameRecord], Int] {

  private val thresholds = Map("red" -> 12, "green" -> 13, "blue" -> 14)

  private def isBelowThreshold(draw: Map[String, Int]): Boolean =
    draw.forall { case (color, amount) =>
      thresholds(color) >= amount
    }

  override def solve(input: Seq[GameRecord]): Int =
    input
      .filter(_.rounds.forall(isBelowThreshold))
      .map(_.id)
      .sum

}
