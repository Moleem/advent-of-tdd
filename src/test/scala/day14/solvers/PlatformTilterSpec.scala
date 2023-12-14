package day14.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object PlatformTilter extends ProblemSolver[String, String] {
  override def solve(input: String): String = {
    val originalColumns = input.split("\n").map(_.toList).toList.transpose
    val tiltedColumns = originalColumns.map { column =>
      column.mkString.split("#", -1).map { partBetweenFixStones =>
        val (spaces, rollingStones) = partBetweenFixStones.partition(_ == 'O')
        spaces.mkString + rollingStones.mkString
      }.mkString("#")
    }

    tiltedColumns.transpose.map(_.mkString).mkString("\n")
  }
}

class PlatformTilterSpec extends AnyFlatSpec with Matchers {

  behavior of PlatformTilter.getClass.getSimpleName

  it should "be able to tilt a platform" in {
    val input =
      """O....#....
        |O.OO#....#
        |.....##...
        |OO.#O....O
        |.O.....O#.
        |O.#..O.#.#
        |..O..#O..O
        |.......O..
        |#....###..
        |#OO..#....""".stripMargin

    val expectedOutput =
      """OOOO.#.O..
        |OO..#....#
        |OO..O##..O
        |O..#.OO...
        |........#.
        |..#....#.#
        |..O..#.O.O
        |..O.......
        |#....###..
        |#....#....""".stripMargin

    PlatformTilter.solve(input) shouldBe expectedOutput
  }
}
