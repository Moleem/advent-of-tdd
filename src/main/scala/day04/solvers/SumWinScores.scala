package day04.solvers

import day04.model.ScratchCardRecord
import utils.ProblemSolver

object SumWinScores extends ProblemSolver[List[ScratchCardRecord], Int] {
  override def solve(input: List[ScratchCardRecord]): Int =
    input
      .map(card => card.lotteryNumbers.intersect(card.ownNumbers))
      .map(_.size)
      .map(size => Math.pow(2, size - 1))
      .map(_.toInt)
      .sum
}
