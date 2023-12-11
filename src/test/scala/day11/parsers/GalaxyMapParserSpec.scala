package day11.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GalaxyMapParserSpec extends AnyFlatSpec with Matchers {

  behavior of GalaxyMapParser.getClass.getName

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

    GalaxyMapParser.parse(input) shouldBe expectedOutput
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

    GalaxyMapParser.parse(input) shouldBe expectedOutput
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

    GalaxyMapParser.parse(input) shouldBe expectedOutput
  }

}
