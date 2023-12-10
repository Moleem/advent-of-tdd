package day09

import day09.parsers.OasisReportParser
import day09.solvers.{OasisRecordExtrapolation, ReverseLists, SumList}
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = OasisReportParser,
    problemSolver = OasisRecordExtrapolation andThen SumList
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = OasisReportParser,
    problemSolver = ReverseLists andThen OasisRecordExtrapolation andThen SumList
  )

}