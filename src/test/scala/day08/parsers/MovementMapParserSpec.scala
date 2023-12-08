package day08.parsers

import day08.model.MovementMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class MovementMapParserSpec extends AnyFlatSpec with Matchers {

  behavior of MovementMapParser.getClass.getName

  it should "parse the movement map correctly" in {
    val input =
      """LLR
        |
        |AAA = (BBB, BBB)
        |BBB = (AAA, ZZZ)
        |ZZZ = (ZZZ, ZZZ)""".stripMargin

    val expectedOutput = MovementMap(
      directions = List('L', 'L', 'R'),
      mappings = Map(
        "AAA" -> ("BBB", "BBB"),
        "BBB" -> ("AAA", "ZZZ"),
        "ZZZ" -> ("ZZZ", "ZZZ")
      )
    )

    MovementMapParser.parse(input) shouldBe expectedOutput
  }

}
