package day08.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ContentParser

case class MovementMap(directions: List[Char], mappings: Map[String, (String, String)])
object MovementMapParser extends ContentParser[MovementMap] {
  override def parse(content: String): MovementMap = ???
}

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
