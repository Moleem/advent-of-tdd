package day11.solvers

import utils.ProblemSolver

object SumDistances extends ProblemSolver[Map[Int, (Int, Int)], Long] {
  override def solve(input: Map[Int, (Int, Int)]): Long = {
    val pairs: Seq[(Int, Int)] =
      (0 to input.keys.max).flatMap { left =>
        (0 to input.keys.max).map { right =>
         (left, right)
        }
      }.filter { case (left, right) => left < right }

    val distances: Seq[Long] = pairs.map { case (left, right) =>
      Math.abs(input(right)._1 - input(left)._1) + Math.abs(input(right)._2 - input(left)._2)
    }

    distances.sum
  }
}
