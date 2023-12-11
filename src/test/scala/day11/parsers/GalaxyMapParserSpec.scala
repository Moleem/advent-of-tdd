package day11.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GalaxyMapParserSpec extends AnyFlatSpec with Matchers {

  behavior of GalaxyMapParser.getClass.getName

  it should "correctly parse a map" in {
    val input = """...#......
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
      1 -> (0, 3),
      2 -> (1, 7),
      3 -> (2, 0),
      4 -> (4, 6),
      5 -> (5, 1),
      6 -> (6, 9),
      7 -> (8, 7),
      8 -> (9, 0),
      9 -> (9, 4),
    )

    GalaxyMapParser.parse(input) shouldBe expectedOutput
  }

}
