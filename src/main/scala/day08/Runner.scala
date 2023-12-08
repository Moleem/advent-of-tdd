package day08

import day08.parsers.MovementMapParser
import day08.solvers.{CountSteps, CountStepsSimultaneously}
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = MovementMapParser,
    problemSolver = CountSteps
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = MovementMapParser,
    problemSolver = CountStepsSimultaneously
  )

}