package day11.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OtherGalaxyMapParserSpec extends AnyFlatSpec with Matchers {

  behavior of OtherGalaxyMapParser.getClass.getName

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

    OtherGalaxyMapParser.parse(input) shouldBe expectedOutput
  }

  it should "correctly expand space along empty rows" in {
    val input =
      """#..
        |...
        |.#.
        |..#""".stripMargin

    val expectedOutput = Map(
      0 -> (0, 0),
      1 -> (1000002, 1),
      2 -> (1000003, 2)
    )

    OtherGalaxyMapParser.parse(input) shouldBe expectedOutput
  }

  it should "correctly expand space along empty columns" in {
    val input =
      """#...
        |..#.
        |...#""".stripMargin

    val expectedOutput = Map(
      0 -> (0, 0),
      1 -> (1, 1000002),
      2 -> (2, 1000003)
    )

    OtherGalaxyMapParser.parse(input) shouldBe expectedOutput
  }


  it should "correctly expand space along both empty rows and columns" in {
    val input =
      """#...
        |....
        |..#.
        |...#""".stripMargin

    val expectedOutput = Map(
      0 -> (0, 0),
      1 -> (1000002, 1000002),
      2 -> (1000003, 1000003)
    )

    OtherGalaxyMapParser.parse(input) shouldBe expectedOutput
  }

}
