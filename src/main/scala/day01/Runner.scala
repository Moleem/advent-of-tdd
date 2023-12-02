package day01

import day01.parsers._
import day01.solvers._
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = NaiveCalibrationParser,
    problemSolver = SumFirstAndLastDigits
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = SpellingAwareCalibrationParser,
    problemSolver = SumFirstAndLastDigits
  )

}