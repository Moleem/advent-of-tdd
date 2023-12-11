package day11.solvers

import utils.ProblemSolver

object SumDistances extends ProblemSolver[Map[Int, (Int, Int)], Int] {
  override def solve(input: Map[Int, (Int, Int)]): Int =
    input(1)._1 - input(0)._1 + input(1)._2 - input(0)._2
}
