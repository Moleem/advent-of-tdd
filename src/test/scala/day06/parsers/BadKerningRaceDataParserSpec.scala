package day06.parsers

import day06.model.RaceData
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ContentParser

object BadKerningRaceDataParser extends ContentParser[RaceData] {
  override def parse(content: String): RaceData = ???
}

class BadKerningRaceDataParserSpec extends AnyFlatSpec with Matchers {

  behavior of "RaceDataParser"

  it should "correctly parse race data" in {
    val input = """Time:      7  15   30
                  |Distance:  9  40  200""".stripMargin

    RaceDataParser.parse(input) shouldBe RaceData(71530, 940200)
  }

}
