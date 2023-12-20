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
    var thisRow = new mutable.HashSet[(Int, Int)]
    var nextRow = new mutable.HashSet[(Int, Int)]
    var lastLineValue = 0L
    var lastLineWasEmpty = true

    println("identifying corners...")
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
    println("corners identified...")

    println("populating intervals...")
    (corners.map(_.row).min to corners.map(_.row).max).foreach { row =>
      if (row % 100000 == 0)
        println(s"${corners.map(_.row).min} < $row < ${corners.map(_.row).max}")

      val cornersOfThisRow = corners.filter(_.row == row).toList.sortBy(_.col)

      cornersOfThisRow.sliding(2).foreach { case List(c1, c2) =>
        (c1.cornerType, c2.cornerType) match {
          case (┌, ┐) =>
            if (!thisRow.exists { case (start, end) => start < c1.col && end > c2.col})
              thisRow.addOne((c1.col, c2.col))
          case (└, ┘) =>
            val intervalsToBeMerged = thisRow.filter { case (start, end) => end == c1.col || start == c2.col}
            if (intervalsToBeMerged.nonEmpty) {
              intervalsToBeMerged.foreach(thisRow.remove)
              thisRow.addOne((intervalsToBeMerged.map(_._1).min, intervalsToBeMerged.map(_._2).max))
            }
          case (└, ┐) =>
            thisRow.find(_._2 == c1.col).foreach { intervalToBeExpanded =>
              thisRow.remove(intervalToBeExpanded)
              thisRow.addOne(intervalToBeExpanded.copy(_2 = c2.col))
            }
          case (┌, ┘) =>
            thisRow.find(_._1 == c2.col).foreach { intervalToBeExpanded =>
              thisRow.remove(intervalToBeExpanded)
              thisRow.addOne(intervalToBeExpanded.copy(_1 = c1.col))
            }
          case _ => // do nothing
        }
      }

      if (row != corners.map(_.row).max) {
        nextRow = new mutable.HashSet[(Int, Int)]
        nextRow.addAll(thisRow)

        cornersOfThisRow.sliding(2).foreach { case List(c1, c2) =>
          (c1.cornerType, c2.cornerType) match {
            case (┌, ┐) =>
              nextRow.find { case (start, end) => start < c1.col && end > c2.col}.foreach { intervalToBeSplit =>
                nextRow.remove(intervalToBeSplit)
                nextRow.addOne(intervalToBeSplit.copy(_2 = c1.col))
                nextRow.addOne(intervalToBeSplit.copy(_1 = c2.col))
              }
            case (└, ┘) =>
              if (nextRow.exists{ case (start, end) => start == c1.col && end == c2.col})
                nextRow.remove((c1.col, c2.col))
            case (└, ┐) =>
              nextRow.find(_._1 == c1.col).foreach { intervalToBeReduced =>
                nextRow.remove(intervalToBeReduced)
                nextRow.addOne(intervalToBeReduced.copy(_1 = c2.col))
              }
            case (┌, ┘) =>
              nextRow.find(_._2 == c2.col).foreach { intervalToBeReduced =>
                nextRow.remove(intervalToBeReduced)
                nextRow.addOne(intervalToBeReduced.copy(_2 = c1.col))
              }
            case _ => // do nothing
          }
        }
      }

      lastLineValue = thisRow.map{ case (start, end) => 1+end-start}.sum
      area += lastLineValue

      thisRow = nextRow
      nextRow = null

      //|-----|------------------|-------------------|
      //|     | modifies current | modifies next     |
      //|-----|------------------|-------------------|
      //|┌  ┐ | add              | add same          |
      //|-----|------------------|-------------------|
      //|┌  ┐ | none             | split             |
      //|-----|------------------|-------------------|
      //|     | none             | add same          |
      //|-----|------------------|-------------------|
      //|└  ┘ | none             | remove            |
      //|-----|------------------|-------------------|
      //|└  ┘ | merge            | add same          |
      //|-----|------------------|-------------------|
      //|└  ┐ | none             | (r) decrease      |
      //|-----|------------------|-------------------|
      //|└  ┐ | (e) increase     | add same          |
      //|-----|------------------|-------------------|
      //|┌  ┘ | none             | (r) decrease      |
      //|-----|------------------|-------------------|
      //|┌  ┘ | (e) increase     | add same          |
      //|-----|------------------|-------------------|
    }

    println("intervals populated...")

    area
  }
}
