package day09.solvers

import utils.ProblemSolver

object ReverseLists extends ProblemSolver[List[List[Int]], List[List[Int]]] {
  override def solve(input: List[List[Int]]): List[List[Int]] = input.map(_.reverse)
}
