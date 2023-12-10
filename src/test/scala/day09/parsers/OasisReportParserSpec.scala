package day09.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OasisReportParserSpec extends AnyFlatSpec with Matchers {

  behavior of OasisReportParser.getClass.getName

  it should "parse number sequences" in {
    val input =
      """0 3 6 9 12 15
        |1 3 6 10 15 21
        |10 13 16 21 30 45""".stripMargin

    val expectedOutput = List(
      List(0, 3, 6, 9, 12, 15),
      List(1, 3, 6, 10, 15, 21),
      List(10, 13, 16, 21, 30, 45),
    )

    OasisReportParser.parse(input) shouldBe expectedOutput
  }
}
