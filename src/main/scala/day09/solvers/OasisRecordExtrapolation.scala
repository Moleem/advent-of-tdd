package day09.solvers

import utils.ProblemSolver

object OasisRecordExtrapolation extends ProblemSolver[List[List[Int]], List[Int]] {

  override def solve(input: List[List[Int]]): List[Int] =
    List(extrapolate(input.head))


  private def extrapolate(input: List[Int]): Int =
    if (input.forall(_ == 0)) 0
    else ???

}
