package day09.solvers

import utils.ProblemSolver

object SumList extends ProblemSolver[List[Int], Int] {
  override def solve(input: List[Int]): Int = input.sum
}
