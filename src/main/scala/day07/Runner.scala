package day07

import day07.parsers.{HandBidParser, JokerAwareHandBidParser}
import day07.solvers.{JokerAwareTotalWinningCalculator, TotalWinningCalculator}
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = HandBidParser,
    problemSolver = TotalWinningCalculator
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = JokerAwareHandBidParser,
    problemSolver = JokerAwareTotalWinningCalculator
  )

}