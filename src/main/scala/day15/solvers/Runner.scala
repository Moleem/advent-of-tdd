package day15.solvers

import utils.PrintSolution
import utils.parsers.StringReader

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = StringReader,
    problemSolver = ???
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = StringReader,
    problemSolver = ???
  )

}