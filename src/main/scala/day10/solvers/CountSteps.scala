package day10.solvers

import utils.ProblemSolver

import scala.annotation.tailrec

object CountSteps extends ProblemSolver[List[List[Char]], Int] {
  override def solve(input: List[List[Char]]): Int = {

    @tailrec
    def findStart(row: Int, col: Int): (Int, Int) = {
      if (input(row)(col) == 'S') (row, col)
      else if (col < input.head.size - 1) findStart(row, col+1)
      else if (row < input.size - 1) findStart(row+1, 0)
      else ??? // we assume there is an S
    }


    def countSteps(
                    prevDirection: String,
                    currentRow: Int,
                    currentCol: Int,
                    currentStepCount: Int
                  ): Int =
      (input(currentRow)(currentCol), prevDirection) match {
        case ('S', direction) if direction.nonEmpty => currentStepCount
        case ('S', direction) if direction.isEmpty =>
          val (southRow, southCol) = getSouthCoordinates(currentRow, currentCol)

          if (canMoveTo(southRow, southCol))
            countSteps("north", southRow, southCol, currentStepCount+1)
          else ???
        case ('|', "north") =>
          val (southRow, southCol) = getSouthCoordinates(currentRow, currentCol)
          countSteps("north", southRow, southCol, currentStepCount+1)
      }

    def getSouthCoordinates(currentRow: Int, currentCol: Int): (Int, Int) =
      (currentRow+1, currentCol)

    def canMoveTo(row: Int, col: Int): Boolean =
      0 <= row && row < input.size &&
      0 <= col && col < input.head.size

    val (startRow, startCol) = findStart(0, 0)
    countSteps("", startRow, startCol, 0)
  }

}
