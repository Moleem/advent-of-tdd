package day07.solvers

import day07.model.HandBid
import utils.ProblemSolver

object TotalWinningCalculator extends ProblemSolver[List[HandBid], Int] {
  override def solve(input: List[HandBid]): Int = {
    input
      .sortBy(_.hand)
      .zipWithIndex
      .map { case (handBid, index) => (handBid, index+1)}
      .map { case (handBid, rank) => handBid.bid * rank }
  }.sum
}
