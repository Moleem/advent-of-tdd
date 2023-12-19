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

    // add interval placeholders
    (corners.map(_.row).min to corners.map(_.row).max).foreach { row =>
      val intervalsForRow = new mutable.HashSet[(Int, Int)]
      intervals.addOne(intervalsForRow)
    }

    // add first row intervals
    val firstRow = intervals.head
    val nextRow = intervals.tail.head
    val firstRowCorners = new ListBuffer[CornerPoint]
    firstRowCorners.addAll(corners.filter(_.row == 0).toList.sortBy(_.col))
    while (firstRowCorners.nonEmpty) {
      val firstCorner = firstRowCorners.head
      val secondCorner = firstRowCorners.tail.head
      (firstCorner.cornerType, secondCorner.cornerType) match {
        case (┌, ┐) =>
          firstRow.addOne((firstCorner.col, secondCorner.col))
          nextRow.addOne((firstCorner.col, secondCorner.col))
      }
      firstRowCorners.remove(0, 2)
    }

    // add last row intervals
    val lastRow = intervals.last
    val lastRowCorners = new ListBuffer[CornerPoint]
    lastRowCorners.addAll(corners.filter(_.row == corners.map(_.row).max).toList.sortBy(_.col))

    while (lastRowCorners.nonEmpty) {
      val firstCorner = lastRowCorners.head
      val secondCorner = lastRowCorners.tail.head
      (firstCorner.cornerType, secondCorner.cornerType) match {
        case (└, ┘) =>
          lastRow.addOne((firstCorner.col, secondCorner.col))
      }
      lastRowCorners.remove(0, 2)
    }

    // middle rows
    (corners.map(_.row).min + 1 until corners.map(_.row).max).foreach { row =>
      val cornersInRow = new ListBuffer[CornerPoint]
      cornersInRow.addAll(corners.filter(_.row == row).toList.sortBy(_.col))

      val thisRow = intervals(row)
      val nextRow = intervals(row+1)

      if (cornersInRow.isEmpty) {
        nextRow.addAll(thisRow)
      } else {
        while (cornersInRow.nonEmpty) {
          val firstCorner = cornersInRow.head
          val secondCorner = cornersInRow.tail.head
          (firstCorner.cornerType, secondCorner.cornerType) match {
            case (┌, ┐) =>
              thisRow.addOne((firstCorner.col, secondCorner.col))
              nextRow.addOne((firstCorner.col, secondCorner.col))

            case (└, ┘) =>
              thisRow.addOne((firstCorner.col, secondCorner.col))

            case (└, ┐) =>
              val intervalToBeReduced = thisRow.find(_._1 == firstCorner.col)
              val reducedInterval = intervalToBeReduced.map(_.copy(_1 = secondCorner.col))
              val intervalToBeExpanded = thisRow.find(_._2 == firstCorner.col)
              val expandedInterval = intervalToBeExpanded.map(_.copy(_2 = secondCorner.col))

              intervalToBeExpanded.foreach(thisRow.remove)
              expandedInterval.foreach(thisRow.addOne)

              nextRow.addAll(thisRow)
              intervalToBeReduced.foreach(nextRow.remove)
              reducedInterval.foreach(nextRow.addOne)

            case (┌, ┘) =>
              val intervalToBeReduced = thisRow.find(_._2 == secondCorner.col)
              val reducedInterval = intervalToBeReduced.map(_.copy(_2 = firstCorner.col))
              val intervalToBeExpanded = thisRow.find(_._1 == secondCorner.col)
              val expandedInterval = intervalToBeExpanded.map(_.copy(_1 = firstCorner.col))

              intervalToBeExpanded.foreach(thisRow.remove)
              expandedInterval.foreach(thisRow.addOne)

              nextRow.addAll(thisRow)
              intervalToBeReduced.foreach(nextRow.remove)
              reducedInterval.foreach(nextRow.addOne)
          }

          cornersInRow.remove(0, 2)
        }
      }

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

    val result = intervals.flatMap(intervalsForRow => intervalsForRow.map{ case (start, end) => 1+end-start}).sum
    result
  }
}
