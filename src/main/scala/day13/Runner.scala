package day13

import day00.parsers._
import day00.solvers._
import day13.parsers.MirrorFinder
import utils.{PrintSolution, ProblemSolver}

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = MirrorFinder,
    problemSolver = (input: Long) => input
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = MirrorFinder,
    problemSolver = (input: Long) => input
  )

}