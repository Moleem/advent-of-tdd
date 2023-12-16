package day15

import day15.solvers.{BoxSummarizer, Boxer, Hasher}
import utils.PrintSolution
import utils.parsers.StringReader

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = StringReader,
    problemSolver = Hasher
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = StringReader,
    problemSolver = Boxer andThen BoxSummarizer
  )

}