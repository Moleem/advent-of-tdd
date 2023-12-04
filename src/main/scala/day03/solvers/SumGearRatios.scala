package day03.solvers

import day03.model.AttachmentRecord
import utils.ProblemSolver

object SumGearRatios extends ProblemSolver[Set[AttachmentRecord], Int] {
  override def solve(input: Set[AttachmentRecord]): Int = {
    input
      .filter(_.shape == '*')
      .filter(_.attachedNumbers.length == 2)
      .map(_.attachedNumbers.product)
      .sum
  }
}
