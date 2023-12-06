package day06.solvers

import day06.model.RaceData
import utils.ProblemSolver

object SumRecordBreakingOptions extends ProblemSolver[List[RaceData], Long] {
  override def solve(input: List[RaceData]): Long = {
    var product = 1L

    input.foreach { race =>
      var waysToWin = 0
      var i = 0
        while (i<race.length) {
          if (i * (race.length - i) > race.recordDistance) {
            waysToWin += 1
          }
          i+=1
        }

      product = product*waysToWin
    }

    product
  }
}
