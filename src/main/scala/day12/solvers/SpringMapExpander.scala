package day12.solvers

import utils.ProblemSolver

object SpringMapExpander extends ProblemSolver[List[(String, List[Int])], List[(String, List[Int])]] {
  override def solve(input: List[(String, List[Int])]): List[(String, List[Int])] =
    input.map { case (pattern, nums) =>
      (List.tabulate(5)(_ => pattern).mkString("?"), List.tabulate(5)(_ => nums).flatten)
    }
}
