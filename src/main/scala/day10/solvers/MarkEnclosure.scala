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

  private def isValidCoordinate(input: List[List[Char]], row: Int, col: Int): Boolean =
    0 <= row && row < input.size &&
      0 <= col && col < input.head.size

  private def hasNorthConnector(input: List[List[Char]], row: Int, col: Int): Boolean =
    isValidCoordinate(input, row, col) &&
    Set('|', 'J', 'L').contains(input(row)(col))

  private def hasWestConnector(input: List[List[Char]], row: Int, col: Int): Boolean =
    isValidCoordinate(input, row, col) &&
    Set('-', 'J', '7').contains(input(row)(col))

  private def hasSouthConnector(input: List[List[Char]], row: Int, col: Int): Boolean =
    isValidCoordinate(input, row, col) &&
    Set('|', 'F', '7').contains(input(row)(col))

  private def hasEastConnector(input: List[List[Char]], row: Int, col: Int): Boolean =
    isValidCoordinate(input, row, col) &&
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
  }
}
