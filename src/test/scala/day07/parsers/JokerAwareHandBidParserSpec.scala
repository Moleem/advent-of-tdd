package day07.parsers

import day07.model.{JokerAwareHand, JokerAwareHandBid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JokerAwareHandBidParserSpec extends AnyFlatSpec with Matchers {
  behavior of "JokerAwareHandBidParser"

  it should "correctly parse hands and bids" in {
    val input = """32T3K 765
                  |T55J5 684
                  |KK677 28
                  |KTJJT 220
                  |QQQJA 483""".stripMargin

    JokerAwareHandBidParser.parse(input) shouldBe List(
      JokerAwareHandBid(JokerAwareHand("32T3K"), 765),
      JokerAwareHandBid(JokerAwareHand("T55J5"), 684),
      JokerAwareHandBid(JokerAwareHand("KK677"), 28),
      JokerAwareHandBid(JokerAwareHand("KTJJT"), 220),
      JokerAwareHandBid(JokerAwareHand("QQQJA"), 483)
    )
  }
}

