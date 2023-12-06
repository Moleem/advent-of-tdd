package day06.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ContentParser

case class RaceData(length: Long, recordDistance: Long) {

}

object RaceDataParser extends ContentParser[List[RaceData]] {
  override def parse(content: String): List[RaceData] = ???
}

class RaceDataParserSpec extends AnyFlatSpec with Matchers {

  behavior of "RaceDataParser"

  it should "correctly parse race data" in {
    val input = """Time:      7  15   30
                  |Distance:  9  40  200""".stripMargin

    RaceDataParser.parse(input) shouldBe List(
      RaceData(7, 9),
      RaceData(15, 40),
      RaceData(30, 200)
    )
  }

}
