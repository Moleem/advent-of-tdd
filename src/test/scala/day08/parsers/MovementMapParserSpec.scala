package day08.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ContentParser

case class MovementMap(directions: List[Char], mappings: Map[String, (String, String)])
object MovementMapParser extends ContentParser[MovementMap] {

  private  def parseDirections(line: String) =
    line.toList

  private def parseMappings(lines: List[String]) =
    lines.map { line =>
      val parts = line.split("=")
      val key = parts.head.trim
      val values = parts(1).trim.stripPrefix("(").stripSuffix(")").split(",").map(_.trim)
      val left = values(0)
      val right = values(1)

      key -> (left, right)
    }.toMap

  override def parse(content: String): MovementMap = {
    val lines = content.split("\n").toList
    val directions = parseDirections(lines.head)
    val mappings = parseMappings(lines.tail.tail)

    MovementMap(directions, mappings)
  }
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
