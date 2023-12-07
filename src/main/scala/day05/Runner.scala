package day05

import day05.parsers.MappingParser
import day05.solvers.{FindLowestLocation, FindLowestLocationWithSeedRanges}
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = MappingParser,
    problemSolver = FindLowestLocation
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = MappingParser,
    problemSolver = FindLowestLocationWithSeedRanges
  )

}