package day14.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.collection.mutable.ListBuffer

case class Stone(var row: Int, var col: Int)

class AdvancedPlatformTilter(cycleDirections: List[Char], cycleCount: Int) extends ProblemSolver[String, String] {

  override def solve(input: String): String = {
    val matrix = input.split("\n").map(_.toList).toList
    val rowCount = matrix.size
    val colCount = matrix.head.size

    val (stableStones, rollingStones) = getStableAndRollingStones(matrix)

    tiltNorth(stableStones, rollingStones, rowCount, colCount)

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

  private def tiltNorth(stableStones: List[Stone], rollingStones: List[Stone], rowCount: Int, colCount: Int): Unit = {
    rollingStones.foreach(stone => stone.row = 0)
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
}
