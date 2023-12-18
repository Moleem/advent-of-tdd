package day18.solvers

import utils.ProblemSolver

object SmarterAreaCounter extends ProblemSolver[List[(Char, Int)], Long] {
  override def solve(input: List[(Char, Int)]): Long = {
    var area = 0L;

    input.foreach { case (direction, distance) =>
      direction match {
        case 'R' => area += (distance+1)
        case 'D' => area *= (distance+1)
        case 'L' =>
        case 'U' =>
      }
    }

    area
  }
}
