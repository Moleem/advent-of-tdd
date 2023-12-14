package day14.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

case class Stone(var row: Int, var col: Int)

class AdvancedPlatformTilter(cycleDirections: List[Char], cycleCount: Int) extends ProblemSolver[String, String] {

  override def solve(input: String): String = {
    val matrix = input.split("\n").map(_.toList).toList
    val rowCount = matrix.size
    val colCount = matrix.head.size

    val (stableStones, rollingStones) = getStableAndRollingStones(matrix)

    cycleDirections.head match {
      case 'N' => tiltNorth(stableStones, rollingStones, rowCount, colCount)
      case 'W' => tiltWest(stableStones, rollingStones, rowCount, colCount)
      case 'S' => tiltSouth(stableStones, rollingStones, rowCount, colCount)
      case 'E' => tiltEast(stableStones, rollingStones, rowCount, colCount)
    }

    (0 until rowCount).map {row =>
      (0 until colCount).map { col =>
        if (stableStones.exists(stone => stone.row == row && stone.col == col)) '#'
        else if (rollingStones.exists(stone => stone.row == row && stone.col == col)) 'O'
        else '.'
      }.mkString
    }.mkString("\n")

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

}

class AdvancedPlatformTilterSpec extends AnyFlatSpec with Matchers {

  behavior of "AdvancedPlatformTilter"

  it should "be able to move a single stone north when there are no obstacles" in {
    val input =
      """...
        |.O.
        |...""".stripMargin

    val expectedOutput =
      """.O.
        |...
        |...""".stripMargin

    new AdvancedPlatformTilter(List('N'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move a single stone west when there are no obstacles" in {
    val input =
      """...
        |.O.
        |...""".stripMargin

    val expectedOutput =
      """...
        |O..
        |...""".stripMargin

    new AdvancedPlatformTilter(List('W'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move a single stone south when there are no obstacles" in {
    val input =
      """...
        |.O.
        |...""".stripMargin

    val expectedOutput =
      """...
        |...
        |.O.""".stripMargin

    new AdvancedPlatformTilter(List('S'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move a single stone east when there are no obstacles" in {
    val input =
      """...
        |.O.
        |...""".stripMargin

    val expectedOutput =
      """...
        |..O
        |...""".stripMargin

    new AdvancedPlatformTilter(List('E'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move a single stone north when there is an obstacle" in {
    val input =
      """.#.
        |...
        |.O.""".stripMargin

    val expectedOutput =
      """.#.
        |.O.
        |...""".stripMargin

    new AdvancedPlatformTilter(List('N'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move a single stone west when there is an obstacle" in {
    val input =
      """...
        |#.O
        |...""".stripMargin

    val expectedOutput =
      """...
        |#O.
        |...""".stripMargin

    new AdvancedPlatformTilter(List('W'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move a single stone south when there is an obstacle" in {
    val input =
      """.O.
        |...
        |.#.""".stripMargin

    val expectedOutput =
      """...
        |.O.
        |.#.""".stripMargin

    new AdvancedPlatformTilter(List('S'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move a single stone east when there is an obstacle" in {
    val input =
      """...
        |O.#
        |...""".stripMargin

    val expectedOutput =
      """...
        |.O#
        |...""".stripMargin

    new AdvancedPlatformTilter(List('E'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move multiple stones north" in {
        val input =
      """.#
        |..
        |O.
        |.O
        |OO""".stripMargin

    val expectedOutput =
      """O#
        |OO
        |.O
        |..
        |..""".stripMargin

    new AdvancedPlatformTilter(List('N'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move multiple stones west" in {
        val input =
      """..O.O
        |#..OO""".stripMargin

    val expectedOutput =
      """OO...
        |#OO..""".stripMargin

    new AdvancedPlatformTilter(List('W'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move multiple stones south" in {
        val input =
      """OO
        |.O
        |O.
        |..
        |.#""".stripMargin

    val expectedOutput =
      """..
        |..
        |.O
        |OO
        |O#""".stripMargin

    new AdvancedPlatformTilter(List('S'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move multiple stones east" in {
    val input =
      """OO...
        |O.O.#""".stripMargin

    val expectedOutput =
      """...OO
        |..OO#""".stripMargin

    new AdvancedPlatformTilter(List('E'), 1).solve(input) shouldBe expectedOutput
  }

  it should "be able to move a stone around" in {
    val input =
      """...
        |.O.
        |...""".stripMargin

    val expectedOutput =
      """...
        |...
        |..O""".stripMargin

    new AdvancedPlatformTilter(List('N', 'W', 'S', 'E'), 1).solve(input) shouldBe expectedOutput
  }
}
