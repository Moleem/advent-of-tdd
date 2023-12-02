package day00.solvers

import day00.model.DummyRecord
import utils.ProblemSolver

object DummyTask1Solver extends ProblemSolver[Seq[DummyRecord], Int] {

  override def solve(input: Seq[DummyRecord]): Int =
    input.map(record => record.content.length).sum

}
