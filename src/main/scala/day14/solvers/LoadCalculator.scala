package day14.solvers

import utils.ProblemSolver

object LoadCalculator extends ProblemSolver[String, Int] {
  override def solve(input: String): Int = {
    input
      .split("\n")
      .toList
      .reverse
      .zipWithIndex
      .map { case (row, rowId) =>
        (rowId + 1) * row.count(_ == 'O')
      }
      .sum
  }
}
