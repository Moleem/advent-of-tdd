package day11

import day11.parsers.GalaxyMapParser
import day11.solvers.SumDistances
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = new GalaxyMapParser(1),
    problemSolver = SumDistances
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = new GalaxyMapParser(999999),
    problemSolver = SumDistances
  )

}