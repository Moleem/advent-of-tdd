package day09.solvers

import utils.ProblemSolver

object OasisRecordExtrapolation extends ProblemSolver[List[List[Int]], List[Int]] {

  override def solve(input: List[List[Int]]): List[Int] =
    input.map(extrapolate)


  private def extrapolate(input: List[Int]): Int =
    if (input.forall(_ == 0)) 0
    else {
      val nextLine =
        (1 until input.size).map(i => input(i) - input(i-1)).toList
      input.last + extrapolate(nextLine)
    }

}
