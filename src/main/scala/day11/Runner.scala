package day11

import day00.parsers._
import day00.solvers._
import day11.parsers.GalaxyMapParser
import day11.solvers.SumDistances
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = GalaxyMapParser,
    problemSolver = SumDistances
  )

//  PrintSolution(
//    inputFileName = s"/$dayNum/input-2.txt",
//    contentParser = DummyTask2Parser,
//    problemSolver = DummyTask2Solver
//  )

}