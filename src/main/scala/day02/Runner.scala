package day02

import day02.parsers._
import day02.solvers._
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = GameRecordParser,
    problemSolver = SumPossibleGameIds
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = GameRecordParser,
    problemSolver = SumPowerMins
  )
//  // 63711


}