package day11.solvers

import utils.ProblemSolver

object SumDistances extends ProblemSolver[Map[Int, (Int, Int)], Int] {
  override def solve(input: Map[Int, (Int, Int)]): Int = {
    val pairs: Seq[(Int, Int)] =
      (0 to input.keys.max).flatMap { left =>
        (0 to input.keys.max).map { right =>
         (left, right)
        }
      }.filter { case (left, right) => left < right }

    val distances = pairs.map { case (left, right) =>
      input(right)._1 - input(left)._1 + input(right)._2 - input(left)._2
    }

    distances.sum
  }
}
