package day10

import day00.parsers._
import day00.solvers._
import day10.parsers.PipeMazeParser
import day10.solvers.CountSteps
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = PipeMazeParser,
    problemSolver = CountSteps andThen((i: Int) => i/2)
  )

//  PrintSolution(
//    inputFileName = s"/$dayNum/input-2.txt",
//    contentParser = DummyTask2Parser,
//    problemSolver = DummyTask2Solver
//  )

}