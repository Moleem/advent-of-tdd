package day06

import day00.parsers._
import day00.solvers._
import day06.parsers.{BadKerningRaceDataParser, RaceDataParser}
import day06.solvers.SumRecordBreakingOptions
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = RaceDataParser,
    problemSolver = SumRecordBreakingOptions
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = BadKerningRaceDataParser,
    problemSolver = SumRecordBreakingOptions
  )

}