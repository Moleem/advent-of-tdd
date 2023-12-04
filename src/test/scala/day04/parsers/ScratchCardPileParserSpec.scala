package day04.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ContentParser

case class ScratchCardRecord(lotteryNumbers: List[Int], ownNumbers: List[Int])

object ScratchCardPileParser extends ContentParser[List[ScratchCardRecord]] {
  override def parse(content: String): List[ScratchCardRecord] =
    content
      .split("\n")
      .map { line => line.split(":")(1) }
      .map { line => line.split("\\|") }
      .map { numListStrings =>
        numListStrings
          .map(_.split(" ")
            .map(_.trim)
            .filter(_.nonEmpty)
            .map(_.toInt)
            .toList
          )}
      .map { lists => ScratchCardRecord(lists(0), lists(1))}
      .toList
}

class ScratchCardPileParserSpec extends AnyFlatSpec with Matchers {

  behavior of "ScratchCardPileParser"

  it should "correctly read both lottery and own lists from multiple lines" in {
    val input = {
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin
    }

    val expectedOutput = List(
      ScratchCardRecord(List(41, 48, 83, 86, 17), List(83, 86,  6, 31, 17,  9, 48, 53)),
      ScratchCardRecord(List(13, 32, 20, 16, 61), List(61, 30, 68, 82, 17, 32, 24, 19)),
      ScratchCardRecord(List( 1, 21, 53, 59, 44), List(69, 82, 63, 72, 16, 21, 14,  1)),
      ScratchCardRecord(List(41, 92, 73, 84, 69), List(59, 84, 76, 51, 58,  5, 54, 83)),
      ScratchCardRecord(List(87, 83, 26, 28, 32), List(88, 30, 70, 12, 93, 22, 82, 36)),
      ScratchCardRecord(List(31, 18, 13, 56, 72), List(74, 77, 10, 23, 35, 67, 36, 11)),
    )

    ScratchCardPileParser.parse(input) shouldBe expectedOutput
  }

}
