package day03

import day03.parsers.{AttachmentParser, SymbolAwareEngineSchemaParser}
import day03.solvers.SumGearRatios
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = SymbolAwareEngineSchemaParser,
    problemSolver = (input: Seq[Int]) => input.sum
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = AttachmentParser,
    problemSolver = SumGearRatios
  )

}