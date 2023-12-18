package day17

import day17.solvers.{MinimizeHeatLoss, MinimizeHeatLossMutable, MinimizeHeatLossMutable2}
import utils.PrintSolution
import utils.parsers.StringReader

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = StringReader,
    problemSolver = MinimizeHeatLossMutable
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = StringReader,
    problemSolver = MinimizeHeatLossMutable2
  )

}