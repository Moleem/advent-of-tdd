package day16

import day16.solvers.EnergizedTileFinder
import utils.{PrintSolution, ProblemSolver}
import utils.parsers.StringReader
import day16.model.{Down, Right, Up, Left}

object Runner extends App {

  val dayNum = this.getClass.getPackageName

  PrintSolution(
    inputFileName = s"/$dayNum/input-1.txt",
    contentParser = StringReader,
    problemSolver =
      new EnergizedTileFinder(0, 0, Right) andThen
      ((input: String) => input.count(_ == '#'))
  )

  PrintSolution(
    inputFileName = s"/$dayNum/input-2.txt",
    contentParser = StringReader,
    problemSolver = (input: String) => {
      val matrix = input.split("\n").map(_.toList).toList
      val rowCount = matrix.size
      val colCount = matrix.head.size

      val possibleStartsFromTop = (0 until colCount).map(c => ((0, c), Down))
      val possibleStartsFromBottom = (0 until colCount).map(c => ((rowCount-1, c), Up))
      val possibleStartsFromLeft = (0 until rowCount).map(r => ((r, 0), Right))
      val possibleStartsFromRight = (0 until rowCount).map(r => ((r, colCount-1), Left))

      val possibleStarts = possibleStartsFromTop ++ possibleStartsFromBottom ++ possibleStartsFromLeft ++ possibleStartsFromRight
      val totalStartCount = possibleStarts.size

      possibleStarts.zipWithIndex.map { case (((row, col), direction), id) =>
        println(s"$id / $totalStartCount")
        new EnergizedTileFinder(row, col, direction).solve(input).count(_=='#')
      }.max
    }
  )

}