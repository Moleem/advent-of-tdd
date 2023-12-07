package day07.parsers

import day07.model.{Hand, HandBid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HandBidParserSpec extends AnyFlatSpec with Matchers {
  behavior of "HandBidParser"

  it should "correctly parse hands and bids" in {
    val input = """32T3K 765
                  |T55J5 684
                  |KK677 28
                  |KTJJT 220
                  |QQQJA 483""".stripMargin

    HandBidParser.parse(input) shouldBe List(
      HandBid(Hand("32T3K"), 765),
      HandBid(Hand("T55J5"), 684),
      HandBid(Hand("KK677"), 28),
      HandBid(Hand("KTJJT"), 220),
      HandBid(Hand("QQQJA"), 483)
    )
  }
}

