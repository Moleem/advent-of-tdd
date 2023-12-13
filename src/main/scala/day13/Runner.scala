package day13

import day13.parsers.{MirrorFinder, SmudgeMirrorFinder}
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = MirrorFinder,
    problemSolver = (input: Long) => input
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = SmudgeMirrorFinder,
    problemSolver = (input: Long) => input
  )

}