package day14.solvers

import day14.model.Stone
import utils.ProblemSolver

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class AdvancedPlatformTilter(cycleDirections: List[Char], cycleCount: Int) extends ProblemSolver[String, String] {

  private val cache = new mutable.HashMap[String, Int]()

  override def solve(input: String): String = {
    val matrix = input.split("\n").map(_.toList).toList
    val rowCount = matrix.size
    val colCount = matrix.head.size

    var solution = ""

    val (stableStones, rollingStones) = getStableAndRollingStones(matrix)

    var solutionFound = false
    var i = 0

    while (i<cycleCount && !solutionFound) {
      if (!solutionFound) {
        val beforeState = backToString(stableStones, rollingStones, rowCount, colCount)
        if (!cache.contains(beforeState)) {
          cycleDirections.foreach {
            case 'N' => tiltNorth(stableStones, rollingStones, rowCount, colCount)
            case 'W' => tiltWest(stableStones, rollingStones, rowCount, colCount)
            case 'S' => tiltSouth(stableStones, rollingStones, rowCount, colCount)
            case 'E' => tiltEast(stableStones, rollingStones, rowCount, colCount)
          }

          solution = backToString(stableStones, rollingStones, rowCount, colCount)
          cache.put(beforeState, i)
        } else {
          solutionFound = true

          val loopStart = cache(backToString(stableStones, rollingStones, rowCount, colCount))
          val loopLength = i-loopStart
          val targetModulo = (cycleCount - loopStart) % loopLength + loopStart

          solution = cache.find { case (key, id) => id == targetModulo }.get._1
        }
      }

      i += 1
    }

    solution
  }

  private def getStableAndRollingStones(matrix: List[List[Char]]): (List[Stone], List[Stone]) = {
    val stableStones = new ListBuffer[Stone]
    val rollingStones = new ListBuffer[Stone]

    matrix.indices.foreach(row =>
      matrix.head.indices.foreach(col =>
        matrix(row)(col) match {
          case '#' => stableStones.addOne(Stone(row, col))
          case 'O' => rollingStones.addOne(Stone(row, col))
          case '.' => // do nothing
        }
      )
    )

    (stableStones.toList, rollingStones.toList)
  }

  private def tiltNorth(stableStones: List[Stone], rollingStones: List[Stone], rowCount: Int, colCount: Int): Unit =
    rollingStones.sortBy(_.row).foreach { stone =>
      val positionOfWall = -1
      val positionsOfStableStonesAbove = stableStones.filter(_.col == stone.col).map(_.row)
      val positionsOfRollingStonesAbove = rollingStones.filter(_.col == stone.col).map(_.row)
      val relevantPositions = positionOfWall :: positionsOfStableStonesAbove ++ positionsOfRollingStonesAbove
      stone.row = relevantPositions.filter(_ < stone.row)
        .map(_ + 1)
        .max
    }

  private def tiltWest(stableStones: List[Stone], rollingStones: List[Stone], rowCount: Int, colCount: Int): Unit =
    rollingStones.sortBy(_.col).foreach { stone =>
      val positionOfWall = -1
      val positionsOfStableStonesBefore = stableStones.filter(_.row == stone.row).map(_.col)
      val positionsOfRollingStonesBefore = rollingStones.filter(_.row == stone.row).map(_.col)
      val relevantPositions = positionOfWall :: positionsOfStableStonesBefore ++ positionsOfRollingStonesBefore
      stone.col = relevantPositions.filter(_ < stone.col)
        .map(_ + 1)
        .max
    }

  private def tiltSouth(stableStones: List[Stone], rollingStones: List[Stone], rowCount: Int, colCount: Int): Unit =
    rollingStones.sortBy(_.row).reverse.foreach { stone =>
      val positionOfWall = rowCount
      val positionsOfStableStonesBelow = stableStones.filter(_.col == stone.col).map(_.row)
      val positionsOfRollingStonesBelow = rollingStones.filter(_.col == stone.col).map(_.row)
      val relevantPositions = positionOfWall :: positionsOfStableStonesBelow ++ positionsOfRollingStonesBelow
      stone.row = relevantPositions.filter(_ > stone.row)
        .map(_ - 1)
        .min
    }

  private def tiltEast(stableStones: List[Stone], rollingStones: List[Stone], rowCount: Int, colCount: Int): Unit =
    rollingStones.sortBy(_.col).reverse.foreach { stone =>
      val positionOfWall = colCount
      val positionsOfStableStonesAfter = stableStones.filter(_.row == stone.row).map(_.col)
      val positionsOfRollingStonesAfter = rollingStones.filter(_.row == stone.row).map(_.col)
      val relevantPositions = positionOfWall :: positionsOfStableStonesAfter ++ positionsOfRollingStonesAfter
      stone.col = relevantPositions.filter(_ > stone.col)
        .map(_ - 1)
        .min
    }

  private def backToString(stableStones: List[Stone], rollingStones: List[Stone], rowCount: Int, colCount: Int): String =
    (0 until rowCount).map { row =>
      (0 until colCount).map { col =>
        if (stableStones.exists(stone => stone.row == row && stone.col == col)) '#'
        else if (rollingStones.exists(stone => stone.row == row && stone.col == col)) 'O'
        else '.'
      }.mkString
    }.mkString("\n")
}
