package day12

import day00.parsers._
import day00.solvers._
import day12.parsers.SpringMapParser
import day12.solvers.SpringErrorMatcher
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = SpringMapParser,
    problemSolver = SpringErrorMatcher
  )

//  PrintSolution(
//    inputFileName = s"/$dayNum/input-2.txt",
//    contentParser = DummyTask2Parser,
//    problemSolver = DummyTask2Solver
//  )

}