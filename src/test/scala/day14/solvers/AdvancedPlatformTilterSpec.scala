package day14.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer


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

  it should "be able to move a stone around in multiple cycles" in {
    val input =
      """#...#.
        |..O...
        |.....#
        |.#....
        |......""".stripMargin

    val expectedOutput =
      """#...#.
        |......
        |.....#
        |.#....
        |.....O""".stripMargin

    new AdvancedPlatformTilter(List('N', 'W', 'S', 'E'), 2).solve(input) shouldBe expectedOutput
  }

  it should "work on the example (1 cycle)" in {
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
      """.....#....
        |....#...O#
        |...OO##...
        |.OO#......
        |.....OOO#.
        |.O#...O#.#
        |....O#....
        |......OOOO
        |#...O###..
        |#..OO#....""".stripMargin

    new AdvancedPlatformTilter(List('N', 'W', 'S', 'E'), 1).solve(input) shouldBe expectedOutput
  }

  it should "work on the example (2 cycles)" in {
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
      """.....#....
        |....#...O#
        |.....##...
        |..O#......
        |.....OOO#.
        |.O#...O#.#
        |....O#...O
        |.......OOO
        |#..OO###..
        |#.OOO#...O""".stripMargin

    new AdvancedPlatformTilter(List('N', 'W', 'S', 'E'), 2).solve(input) shouldBe expectedOutput
  }

  it should "work on the example (3 cycles)" in {
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
      """.....#....
        |....#...O#
        |.....##...
        |..O#......
        |.....OOO#.
        |.O#...O#.#
        |....O#...O
        |.......OOO
        |#...O###.O
        |#.OOO#...O""".stripMargin

    new AdvancedPlatformTilter(List('N', 'W', 'S', 'E'), 3).solve(input) shouldBe expectedOutput
  }

}
