package day06.solvers

import day06.model.RaceData
import utils.ProblemSolver

object SumRecordBreakingOptions extends ProblemSolver[List[RaceData], Long] {
  override def solve(input: List[RaceData]): Long = {
    input.map { race =>
      (0 to race.length).count(pressTime =>
        pressTime * (race.length - pressTime) > race.recordDistance
      )
    }.product
  }
}
