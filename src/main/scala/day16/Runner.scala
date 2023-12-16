package day16

import day16.solvers.EnergizedTileFinder
import utils.{PrintSolution, ProblemSolver}
import utils.parsers.StringReader
import day16.model.Right

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = StringReader,
    problemSolver =
      new EnergizedTileFinder(0, 0, Right) andThen
      ((input: String) => input.count(_ == '#'))
  )

//  PrintSolution(
//    inputFileName = s"/$dayNum/input-2.txt",
//    contentParser = DummyTask2Parser,
//    problemSolver = DummyTask2Solver
//  )

}