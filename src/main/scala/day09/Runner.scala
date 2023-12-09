package day09

import day00.parsers._
import day00.solvers._
import day09.parsers.OasisReportParser
import day09.solvers.OasisRecordExtrapolation
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = OasisReportParser,
    problemSolver = OasisRecordExtrapolation.andThen((extrapolations: List[Int]) => extrapolations.sum)
  )

//  PrintSolution(
//    inputFileName = s"/$dayNum/input-2.txt",
//    contentParser = DummyTask2Parser,
//    problemSolver = DummyTask2Solver
//  )

}