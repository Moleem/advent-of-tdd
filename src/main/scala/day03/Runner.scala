package day03

import day00.parsers._
import day00.solvers._
import day03.parsers.SymbolAwareEngineSchemaParser
import utils.{PrintSolution, ProblemSolver}

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = SymbolAwareEngineSchemaParser,
    problemSolver = (input: Seq[Int]) => input.sum
  )

//  PrintSolution(
//    inputFileName = s"/$dayNum/input-2.txt",
//    contentParser = DummyTask2Parser,
//    problemSolver = DummyTask2Solver
//  )

}