package day09.solvers

import utils.ProblemSolver

object ReverseList extends ProblemSolver[List[Int], List[Int]] {
  override def solve(input: List[Int]): List[Int] = input.reverse
}
