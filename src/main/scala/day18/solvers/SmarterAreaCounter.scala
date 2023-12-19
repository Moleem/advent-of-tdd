package day18.solvers

import utils.ProblemSolver

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object SmarterAreaCounter extends ProblemSolver[List[(Char, Int)], Long] {

  sealed trait CornerType
  case object └ extends CornerType
  case object ┘ extends CornerType
  case object ┐ extends CornerType
  case object ┌ extends CornerType

  case class CornerPoint(row: Int, col: Int, cornerType: CornerType)

  override def solve(input: List[(Char, Int)]): Long = {
    var area = 0L;
    var currentCol = 0;
    var currentRow = 0;
    var previousDirection: Char = input.last._1
    val corners = new ListBuffer[CornerPoint]
    val intervals = new ListBuffer[mutable.HashSet[(Int, Int)]]

    input.foreach { case (direction, distance) =>
      direction match {
        case 'R' =>
          previousDirection match {
            case 'U' =>
              corners.addOne(CornerPoint(currentRow, currentCol, ┌))
            case 'D' =>
              corners.addOne(CornerPoint(currentRow, currentCol, └))
          }

          currentCol += distance

        case 'D' =>
          previousDirection match {
            case 'L' =>
              corners.addOne(CornerPoint(currentRow, currentCol, ┌))
            case 'R' =>
              corners.addOne(CornerPoint(currentRow, currentCol, ┐))
          }

          currentRow += distance

        case 'L' =>
          previousDirection match {
            case 'U' =>
              corners.addOne(CornerPoint(currentRow, currentCol, ┐))
            case 'D' =>
              corners.addOne(CornerPoint(currentRow, currentCol, ┘))
          }

          currentCol -= distance

        case 'U' =>
          previousDirection match {
            case 'L' =>
              corners.addOne(CornerPoint(currentRow, currentCol, └))
            case 'R' =>
              corners.addOne(CornerPoint(currentRow, currentCol, ┘))
          }

          currentRow -= distance
      }

      previousDirection = direction
    }

    (corners.map(_.row).min to corners.map(_.row).max).foreach { row =>
      val cornersInRow = new ListBuffer[CornerPoint]
      cornersInRow.addAll(corners.filter(_.row == row).toList.sortBy(_.col))

      val intervalsForRow = new mutable.HashSet[(Int, Int)]

      if (row != 0)
        intervalsForRow.addAll(intervals(row-1))

      while (cornersInRow.nonEmpty) {
        val firstCorner = cornersInRow.head
        val secondCorner = cornersInRow.tail.head
        (firstCorner.cornerType, secondCorner.cornerType) match {
          case (┌, ┐) =>
            intervalsForRow.addOne((firstCorner.col, secondCorner.col))
          case (└, ┘) =>
            intervalsForRow.addOne((firstCorner.col, secondCorner.col))
        }


        cornersInRow.remove(0, 2)
      }

      intervals.addOne(intervalsForRow)

      // ┌ ┐
      //  - if there are no previous intervals overlapping,
      //    then starts a new interval
      //  - if it falls into an existing interval,
      //    then it splits the interval in the next round

      // └ ┐
      //  - if the first corner aligns with an interval start,
      //    then that interval will start from the second corner in the next round
      //  - if the first corner aligns with an interval end,
      //    then that interval will end from the second corner in the next round

      // ┌ ┘
      //  - if the second corner aligns with an interval start,
      //    then that interval will start from the first corner in the next round
      //  - if the second corner aligns with an interval end,
      //    then that interval will end from the first corner in the next round

      // └ ┘
      //  - if matches an existing interval,
      //    then that interval will not appear in the next round
      //  - if bridges two existing intervals,
      //    then those intervals will be merged in the next round
    }

    intervals.flatMap(intervalsForRow => intervalsForRow.map{ case (start, end) => 1+end-start}).sum
  }
}
