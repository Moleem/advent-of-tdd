package day09.solvers

import utils.ProblemSolver

object OasisRecordExtrapolation extends ProblemSolver[List[List[Int]], List[Int]] {

  override def solve(input: List[List[Int]]): List[Int] = {
    val firstInput = input.head

    if (firstInput.forall(_ == 0)) List(0)
    else ???
  }

}
