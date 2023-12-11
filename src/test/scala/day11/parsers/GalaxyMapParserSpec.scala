package day11.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GalaxyMapParserSpec extends AnyFlatSpec with Matchers {

  behavior of "GalaxyMapParser"

  it should "correctly parse a map" in {
    val input =
      """#..
        |.#.
        |..#""".stripMargin

    val expectedOutput = Map(
      0 -> (0, 0),
      1 -> (1, 1),
      2 -> (2, 2)
    )

    new GalaxyMapParser(1).parse(input) shouldBe expectedOutput
  }

  it should "correctly expand space along empty rows" in {
    val input =
      """#..
        |...
        |.#.
        |..#""".stripMargin

    val expectedOutput = Map(
      0 -> (0, 0),
      1 -> (3, 1),
      2 -> (4, 2)
    )

    new GalaxyMapParser(1).parse(input) shouldBe expectedOutput
  }

  it should "correctly expand space along empty columns" in {
    val input =
      """#...
        |..#.
        |...#""".stripMargin

    val expectedOutput = Map(
      0 -> (0, 0),
      1 -> (1, 3),
      2 -> (2, 4)
    )

    new GalaxyMapParser(1).parse(input) shouldBe expectedOutput
  }

  it should "correctly expand space along both empty rows and columns" in {
    val input =
      """#...
        |....
        |..#.
        |...#""".stripMargin

    val expectedOutput = Map(
      0 -> (0, 0),
      1 -> (3, 3),
      2 -> (4, 4)
    )

    new GalaxyMapParser(1).parse(input) shouldBe expectedOutput
  }

  it should "behave correctly example 1 - no expansion" in {
    val input =
      """...#......
        |.......#..
        |#.........
        |..........
        |......#...
        |.#........
        |.........#
        |..........
        |.......#..
        |#...#.....""".stripMargin

    val expectedOutput = Map(
      0 -> (0, 3),
      1 -> (1, 7),
      2 -> (2, 0),
      3 -> (4, 6),
      4 -> (5, 1),
      5 -> (6, 9),
      6 -> (8, 7),
      7 -> (9, 0),
      8 -> (9, 4),
    )

    new GalaxyMapParser(0).parse(input) shouldBe expectedOutput
  }

}
