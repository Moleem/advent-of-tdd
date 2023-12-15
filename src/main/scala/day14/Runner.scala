package day14

import day14.solvers.{AdvancedPlatformTilter, LoadCalculator, PlatformTilter}
import utils.PrintSolution
import utils.parsers.StringReader

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = StringReader,
    problemSolver = PlatformTilter andThen LoadCalculator
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = StringReader,
    problemSolver = new AdvancedPlatformTilter(List('N', 'W', 'S', 'E'), 1000000000) andThen LoadCalculator
  )

}