package day12.solvers

import utils.ProblemSolver

object SpringMapExpander extends ProblemSolver[List[(String, List[Int])], List[(String, List[Int])]] {

  private val multiplier = 5

  override def solve(input: List[(String, List[Int])]): List[(String, List[Int])] =
    input.map { case (pattern, nums) =>
      (List.tabulate(multiplier)(_ => pattern).mkString("?"), List.tabulate(multiplier)(_ => nums).flatten)
    }
}
