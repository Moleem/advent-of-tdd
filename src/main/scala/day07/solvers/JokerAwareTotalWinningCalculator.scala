package day07.solvers

import day07.model.JokerAwareHandBid
import utils.ProblemSolver


object JokerAwareTotalWinningCalculator extends ProblemSolver[List[JokerAwareHandBid], Int] {
  override def solve(input: List[JokerAwareHandBid]): Int = {
    input
      .sortBy(_.hand)
      .zipWithIndex
      .map { case (handBid, index) => (handBid, index+1)}
      .map { case (handBid, rank) => handBid.bid * rank }
  }.sum
}
