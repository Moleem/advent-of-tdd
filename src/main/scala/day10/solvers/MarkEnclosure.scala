package day10.solvers

import utils.ProblemSolver

import scala.annotation.tailrec
import scala.collection.immutable.{List, Set}

object MarkEnclosure extends ProblemSolver[List[List[Char]], List[List[Char]]] {

  override def solve(input: List[List[Char]]): List[List[Char]] = {
    val (startRow, startCol) = findStartCoordinates(input)
    val startPipe = determineStartPipeType(input, startRow, startCol)
    val inputWithProperStartPipe = replace(input, startRow, startCol, startPipe)
    val mainPipeCoordinates = getMainPipeCoordinates(inputWithProperStartPipe, startRow, startCol)
    val inputWithNonMainPipesErased = replaceExcept(inputWithProperStartPipe, mainPipeCoordinates)
    val inputWithBorder = addBorder(inputWithNonMainPipesErased)
    val nonEnclosedErased = eraseNonEnclosed(inputWithBorder)

    nonEnclosedErased

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

  private def addBorder(input: List[List[Char]]): List[List[Char]] =
    (0 until input.size+2).map { row =>
      (0 until input.head.size+2).map { col =>
        if (row == 0 || col == 0 || row == input.size+1 || col == input.head.size+1) ' '
        else input(row-1)(col-1)
      }.toList
    }.toList

  private def replaceExcept(input: List[List[Char]], mainPipes: Set[(Int, Int)]): List[List[Char]] =
    input.indices.map { row =>
      input.head.indices.map { col =>
        if (mainPipes.contains((row, col))) input(row)(col)
        else 'X'
      }.toList
    }.toList

  private def erase(input: List[List[Char]], toBeReplaced: Set[(Int, Int)]): List[List[Char]] =
    input.indices.map { row =>
      input.head.indices.map { col =>
        if (toBeReplaced.contains((row, col))) ' '
        else input(row)(col)
      }.toList
    }.toList

  private def determineStartPipeType(input: List[List[Char]], row: Int, col: Int): Char = {
    val isOpenToNorth = hasSouthConnector(input, row-1, col)
    val isOpenToWest = hasEastConnector(input, row, col-1)
    val isOpenToSouth = hasNorthConnector(input, row+1, col)
    val isOpenToEast = hasWestConnector(input, row, col+1)

    (isOpenToNorth, isOpenToWest, isOpenToSouth, isOpenToEast) match {
      case (true, true, true, false) => 'J'
      case (true, false, true, false) => '|'
      case (true, false, false, true) => 'L'
      case (false, true, true, false) => '7'
      case (false, true, false, true) => '-'
      case (false, false, true, true) => 'F'
    }
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

    @tailrec
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

  private def eraseNonEnclosed(input: List[List[Char]]): List[List[Char]] = {
    @tailrec
    def helper(toBeChecked: List[(Int, Int)], alreadyChecked: Set[(Int, Int)], toBeErased: Set[(Int, Int)]): Set[(Int, Int)] = {
      toBeChecked match {
        case Nil => toBeErased
        case head :: tail =>
          val (row, col) = head
          val neighbors = Set((row-1, col), (row+1, col), (row, col-1), (row, col+1))
          val validNeighbors = neighbors.filter { case (r, c) => isValidCoordinate(input, r, c)}
          val nonCheckedValidNeighbors = validNeighbors.diff(alreadyChecked)

          val newToBeChecked =
            if (Set('X', ' ').contains(input(row)(col)))
              (nonCheckedValidNeighbors.toList ++ tail).distinct
            else tail
          val newAlreadyChecked = Set(head) ++ alreadyChecked
          val newToBeErased =
            if (Set('X', ' ').contains(input(row)(col)))
              Set(head) ++ toBeErased
            else toBeErased

          helper(newToBeChecked, newAlreadyChecked, newToBeErased)

      }
    }

    val coordinatesToBeErased = helper(List((0, 0)), Set.empty, Set.empty)

    erase(input, coordinatesToBeErased)
  }
}
