package day07.solvers

import day07.model.{Hand, HandBid, JokerAwareHand, JokerAwareHandBid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JokerAwareTotalWinningCalculatorSpec extends AnyFlatSpec with Matchers {

  behavior of "JokerAwareTotalWinningCalculator"

  it should "multiply rank with bid" in {
    val input = List(JokerAwareHandBid(JokerAwareHand("AAA22"), 25)) // only one hand, so rank 1

    JokerAwareTotalWinningCalculator.solve(input) shouldBe 25
  }

  it should "sum rank-bid products" in {
    val input = List(
      JokerAwareHandBid(JokerAwareHand("32T3K"), 765), // rank 1
      JokerAwareHandBid(JokerAwareHand("T55J5"), 684), // rank 3
      JokerAwareHandBid(JokerAwareHand("KK677"), 28),  // rank 2
      JokerAwareHandBid(JokerAwareHand("KTJJT"), 220), // rank 5
      JokerAwareHandBid(JokerAwareHand("QQQJA"), 483)  // rank 4
    )

    JokerAwareTotalWinningCalculator.solve(input) shouldBe 5905 // 765 * 1 + 28 * 2 + 684 * 3 + 483 * 4 + 220 * 5
  }
}
