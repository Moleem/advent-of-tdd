package day07.solvers

import day07.model.{Hand, HandBid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TotalWinningCalculatorSpec extends AnyFlatSpec with Matchers {

  behavior of "TotalWinningCalculator"

  it should "multiply rank with bid" in {
    val input = List(HandBid(Hand("AAA22"), 25)) // only one hand, so rank 1

    TotalWinningCalculator.solve(input) shouldBe 25
  }

  it should "sum rank-bid products" in {
    val input = List(
      HandBid(Hand("32T3K"), 765), // rank 1
      HandBid(Hand("T55J5"), 684), // rank 4
      HandBid(Hand("KK677"), 28),  // rank 3
      HandBid(Hand("KTJJT"), 220), // rank 2
      HandBid(Hand("QQQJA"), 483)  // rank 5
    )

    TotalWinningCalculator.solve(input) shouldBe 6440 // 765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5
  }
}
