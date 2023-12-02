package day00

import day00.parsers._
import day00.solvers._
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = DummyTask1Parser,
    problemSolver = DummyTask1Solver
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = DummyTask2Parser,
    problemSolver = DummyTask2Solver
  )

}