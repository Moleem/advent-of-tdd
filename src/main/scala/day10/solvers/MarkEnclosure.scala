package day10.solvers

import utils.ProblemSolver

import scala.annotation.tailrec

object MarkEnclosure extends ProblemSolver[List[List[Char]], List[List[Char]]] {

  override def solve(input: List[List[Char]]): List[List[Char]] = {
    val rowCount = input.size + 2
    val colCount = input.head.size + 2

    val (startRow, startCol) = findStartCoordinates(input)
    val startPipe = determineStartPipeType(input, startRow, startCol)
    val inputWithProperStartPipe = replace(input, startRow, startCol, startPipe)
    val mainPipeCoordinates = getMainPipeCoordinates(inputWithProperStartPipe, startRow, startCol)

    (0 until rowCount).map { row =>
      (0 until colCount).map { col =>
        if (mainPipeCoordinates.contains(row-1, col-1))
          inputWithProperStartPipe(row-1)(col-1)
        else
          ' '
      }.toList
    }.toList

  }

  @tailrec
  private def findStartCoordinates(input: List[List[Char]], row: Int = 0, col: Int = 0): (Int, Int) = {
    if (input(row)(col) == 'S') (row, col)
    else if (col == input.head.size-1) findStartCoordinates(input, row+1, 0)
    else findStartCoordinates(input, row, col+1)
  }

  private def replace(input: List[List[Char]], replaceRow: Int, replaceCol: Int, replaceChar: Char): List[List[Char]] =
    input.indices.map { row =>
      input.head.indices.map { col =>
        if (row == replaceRow && col == replaceCol) replaceChar
        else input(row)(col)
      }.toList
    }.toList

  private def determineStartPipeType(input: List[List[Char]], row: Int, col: Int): Char = {
    val isOpenToNorth = hasSouthConnector(input, row-1, col)
    val isOpenToWest = hasEastConnector(input, row, col-1)
    val isOpenToSouth = hasNorthConnector(input, row+1, col)
    val isOpenToEast = hasWestConnector(input, row, col+1)

    if (isOpenToNorth && isOpenToWest) 'J'
    else if (isOpenToNorth && isOpenToSouth) '|'
    else if (isOpenToNorth && isOpenToEast) 'L'
    else if (isOpenToWest && isOpenToSouth) '7'
    else if (isOpenToWest && isOpenToEast) '-'
    else if (isOpenToSouth && isOpenToEast) 'F'
    else ??? // not possible
  }

  private def hasNorthConnector(input: List[List[Char]], row: Int, col: Int): Boolean =
    0 <= row && row < input.size &&
    0 <= col && col < input.head.size &&
    Set('|', 'J', 'L').contains(input(row)(col))

  private def hasWestConnector(input: List[List[Char]], row: Int, col: Int): Boolean =
    0 <= row && row < input.size &&
    0 <= col && col < input.head.size &&
    Set('-', 'J', '7').contains(input(row)(col))

  private def hasSouthConnector(input: List[List[Char]], row: Int, col: Int): Boolean =
    0 <= row && row < input.size &&
    0 <= col && col < input.head.size &&
    Set('|', 'F', '7').contains(input(row)(col))

  private def hasEastConnector(input: List[List[Char]], row: Int, col: Int): Boolean =
    0 <= row && row < input.size &&
    0 <= col && col < input.head.size &&
    Set('-', 'F', 'L').contains(input(row)(col))

  private def getMainPipeCoordinates(input: List[List[Char]], startRow: Int, startCol: Int): Set[(Int, Int)] = {

    def collectCoordinates(row: Int, col: Int, from: String, accumulator: Set[(Int, Int)]): Set[(Int, Int)] = {
      if (accumulator.contains((row, col))) accumulator
      else {
        (input(row)(col), from) match {
          case ('|', "north") => collectCoordinates(row+1, col, "north", Set((row, col)) ++ accumulator)
          case ('|', "south") => collectCoordinates(row-1, col, "south", Set((row, col)) ++ accumulator)
          case ('-', "west")  => collectCoordinates(row, col+1, "west", Set((row, col)) ++ accumulator)
          case ('-', "east")  => collectCoordinates(row, col-1, "east", Set((row, col)) ++ accumulator)
          case ('J', "north") => collectCoordinates(row, col-1, "east", Set((row, col)) ++ accumulator)
          case ('J', "west")  => collectCoordinates(row-1, col, "south", Set((row, col)) ++ accumulator)
          case ('L', "north") => collectCoordinates(row, col+1, "west", Set((row, col)) ++ accumulator)
          case ('L', "east")  => collectCoordinates(row-1, col, "south", Set((row, col)) ++ accumulator)
          case ('7', "south") => collectCoordinates(row, col-1, "east", Set((row, col)) ++ accumulator)
          case ('7', "west")  => collectCoordinates(row+1, col, "north", Set((row, col)) ++ accumulator)
          case ('F', "south") => collectCoordinates(row, col+1, "west", Set((row, col)) ++ accumulator)
          case ('F', "east")  => collectCoordinates(row+1, col, "north", Set((row, col)) ++ accumulator)
        }
      }
    }

    val startDirection = input(startRow)(startCol) match {
      case '|' => "north"
      case '-' => "west"
      case 'J' => "north"
      case 'L' => "north"
      case '7' => "west"
      case 'F' => "east"
    }

    collectCoordinates(startRow, startCol, startDirection, Set.empty)







//    @tailrec
//    def findStart(row: Int, col: Int): (Int, Int) = {
//      if (input(row)(col) == 'S') (row, col)
//      else if (col < input.head.size - 1) findStart(row, col + 1)
//      else if (row < input.size - 1) findStart(row + 1, 0)
//      else ??? // we assume there is an S
//    }
//
//
//    def countSteps(
//                    prevDirection: String,
//                    currentRow: Int,
//                    currentCol: Int,
//                    currentStepCount: Int
//                  ): Int =
//      (input(currentRow)(currentCol), prevDirection) match {
//        case ('S', direction) if direction.nonEmpty => currentStepCount
//        case ('S', direction) if direction.isEmpty =>
//          val (southRow, southCol) = getSouthCoordinates(currentRow, currentCol)
//          val (eastRow, eastCol) = getEastCoordinates(currentRow, currentCol)
//          val (northRow, northCol) = getNorthCoordinates(currentRow, currentCol)
//          val (westRow, westCol) = getWestCoordinates(currentRow, currentCol)
//
//          if (canMoveToSouth(southRow, southCol))
//            countSteps("north", southRow, southCol, currentStepCount + 1)
//          else if (canMoveToEast(eastRow, eastCol))
//            countSteps("west", eastRow, eastCol, currentStepCount + 1)
//          else if (canMoveToNorth(northRow, northCol))
//            countSteps("south", northRow, northCol, currentStepCount + 1)
//          else if (canMoveToWest(westRow, westCol))
//            countSteps("east", westRow, westCol, currentStepCount + 1)
//          else ???
//        case ('|', "north") =>
//          val (southRow, southCol) = getSouthCoordinates(currentRow, currentCol)
//          countSteps("north", southRow, southCol, currentStepCount + 1)
//        case ('|', "south") =>
//          val (northRow, northCol) = getNorthCoordinates(currentRow, currentCol)
//          countSteps("south", northRow, northCol, currentStepCount + 1)
//        case ('L', "north") =>
//          val (eastRow, eastCol) = getEastCoordinates(currentRow, currentCol)
//          countSteps("west", eastRow, eastCol, currentStepCount + 1)
//        case ('L', "east") =>
//          val (northRow, northCol) = getNorthCoordinates(currentRow, currentCol)
//          countSteps("south", northRow, northCol, currentStepCount + 1)
//        case ('J', "north") =>
//          val (westRow, westCol) = getWestCoordinates(currentRow, currentCol)
//          countSteps("east", westRow, westCol, currentStepCount + 1)
//        case ('J', "west") =>
//          val (northRow, northCol) = getNorthCoordinates(currentRow, currentCol)
//          countSteps("south", northRow, northCol, currentStepCount + 1)
//        case ('-', "west") =>
//          val (eastRow, eastCol) = getEastCoordinates(currentRow, currentCol)
//          countSteps("west", eastRow, eastCol, currentStepCount + 1)
//        case ('-', "east") =>
//          val (westRow, westCol) = getWestCoordinates(currentRow, currentCol)
//          countSteps("east", westRow, westCol, currentStepCount + 1)
//        case ('7', "south") =>
//          val (westRow, westCol) = getWestCoordinates(currentRow, currentCol)
//          countSteps("east", westRow, westCol, currentStepCount + 1)
//        case ('7', "west") =>
//          val (southRow, southCol) = getSouthCoordinates(currentRow, currentCol)
//          countSteps("north", southRow, southCol, currentStepCount + 1)
//        case ('F', "east") =>
//          val (southRow, southCol) = getSouthCoordinates(currentRow, currentCol)
//          countSteps("north", southRow, southCol, currentStepCount + 1)
//        case ('F', "south") =>
//          val (eastRow, eastCol) = getEastCoordinates(currentRow, currentCol)
//          countSteps("west", eastRow, eastCol, currentStepCount + 1)
//        case _ => ???
//      }
//
//    def getSouthCoordinates(currentRow: Int, currentCol: Int): (Int, Int) =
//      (currentRow + 1, currentCol)
//
//    def getEastCoordinates(currentRow: Int, currentCol: Int): (Int, Int) =
//      (currentRow, currentCol + 1)
//
//    def getNorthCoordinates(currentRow: Int, currentCol: Int): (Int, Int) =
//      (currentRow - 1, currentCol)
//
//    def getWestCoordinates(currentRow: Int, currentCol: Int): (Int, Int) =
//      (currentRow, currentCol - 1)
//
//    def canMoveToSouth(row: Int, col: Int): Boolean =
//      0 <= row && row < input.size &&
//        0 <= col && col < input.head.size &&
//        Set('|', 'J', 'L').contains(input(row)(col))
//
//    def canMoveToEast(row: Int, col: Int): Boolean =
//      0 <= row && row < input.size &&
//        0 <= col && col < input.head.size &&
//        Set('-', 'J', '7').contains(input(row)(col))
//
//    def canMoveToNorth(row: Int, col: Int): Boolean =
//      0 <= row && row < input.size &&
//        0 <= col && col < input.head.size &&
//        Set('-', 'F', 'L').contains(input(row)(col))
//
//    def canMoveToWest(row: Int, col: Int): Boolean =
//      0 <= row && row < input.size &&
//        0 <= col && col < input.head.size &&
//        Set('-', '7', 'J').contains(input(row)(col))
//
//
//    val (startRow, startCol) = findStart(0, 0)
//    countSteps("", startRow, startCol, 0)
  }
}
