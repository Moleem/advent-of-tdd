package day18

import day17.solvers.{MinimizeHeatLossMutable, MinimizeHeatLossMutable2}
import day18.solvers.{HoleExteriorDigger, HoleInteriorDigger}
import utils.PrintSolution
import utils.parsers.StringReader

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = StringReader,
    problemSolver = HoleExteriorDigger andThen HoleInteriorDigger andThen ((s: String) => s.count(_=='#'))
  )

//  PrintSolution(
//    inputFileName = s"/$dayNum/input-2.txt",
//    contentParser = StringReader,
//    problemSolver = MinimizeHeatLossMutable2
//  )

}