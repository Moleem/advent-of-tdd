package day04

import day04.parsers.ScratchCardPileParser
import day04.solvers.SumWinScores
import utils.PrintSolution

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = ScratchCardPileParser,
    problemSolver = SumWinScores
  )

//  PrintSolution(
//    inputFileName = s"/$dayNum/input-2.txt",
//    contentParser = DummyTask2Parser,
//    problemSolver = DummyTask2Solver
//  )

}