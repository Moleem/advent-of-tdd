package day06.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.{ContentParser, ProblemSolver}

sealed trait Card {
  val strength: Int
  def compare(other: Card): Int = Integer.compare(this.strength, other.strength)
}

case object C_A extends Card {override val strength: Int = 13 }
case object C_K extends Card { override val strength: Int = 12 }
case object C_Q extends Card { override val strength: Int = 11 }
case object C_J extends Card { override val strength: Int = 10 }
case object C_T extends Card { override val strength: Int = 9 }
case object C_9 extends Card { override val strength: Int = 8 }
case object C_8 extends Card { override val strength: Int = 7 }
case object C_7 extends Card { override val strength: Int = 6 }
case object C_6 extends Card { override val strength: Int = 5 }
case object C_5 extends Card { override val strength: Int = 4 }
case object C_4 extends Card { override val strength: Int = 3 }
case object C_3 extends Card { override val strength: Int = 2 }
case object C_2 extends Card { override val strength: Int = 1 }

case class Hand(cards: List[Card]) extends Ordered[Hand] {
  private val counts: List[Int] = cards.groupBy(_.strength).values.map(_.size).toList.sorted.reverse

  private val handRanks = Map(
    List(5) ->   7,
    List(4, 1) -> 6,
    List(3, 2) -> 5,
    List(3, 1, 1) -> 4,
    List(2, 2, 1) -> 3,
    List(2, 1, 1, 1) -> 2,
    List(1, 1, 1, 1, 1) -> 1
  )

  def compare(other: Hand): Int = {
    val handStrengthComparison = Integer.compare(handRanks(this.counts), handRanks(other.counts))
    if (handStrengthComparison != 0) handStrengthComparison
    else
      this.cards.zip(other.cards)
        .map { case (thisCard, otherCard) => thisCard compare otherCard }
        .find(_ != 0)
        .getOrElse(0)
  }
}

object Hand {
  def apply(s: String): Hand =
    Hand(
      s.toList
        .map {
          case 'A' => C_A
          case 'K' => C_K
          case 'Q' => C_Q
          case 'J' => C_J
          case 'T' => C_T
          case '9' => C_9
          case '8' => C_8
          case '7' => C_7
          case '6' => C_6
          case '5' => C_5
          case '4' => C_4
          case '3' => C_3
          case '2' => C_2
        }
    )
}

case class HandBid(hand: Hand, bid: Int)

object HandBidParser extends ContentParser[List[HandBid]] {
  override def parse(content: String): List[HandBid] =
    content.split("\n").toList
      .map{line =>
        val parts = line.split(" ")
        val hand = Hand(parts(0))
        val bid = parts(1).toInt
        HandBid(hand, bid)
      }
}

object TotalWinningCalculator extends ProblemSolver[List[HandBid], Int] {
  override def solve(input: List[HandBid]): Int = {
    input
      .sortBy(_.hand)
      .zipWithIndex
      .map { case (handBid, index) => (handBid, index+1)}
      .map { case (handBid, rank) => handBid.bid * rank }
  }.sum
}

class CardSpec extends AnyFlatSpec with Matchers {
  behavior of "Card"

  it should "have a defined order" in {
    C_A compare C_K shouldBe 1
    C_K compare C_Q shouldBe 1
    C_Q compare C_J shouldBe 1
    C_J compare C_T shouldBe 1
    C_T compare C_9 shouldBe 1
    C_9 compare C_8 shouldBe 1
    C_8 compare C_7 shouldBe 1
    C_7 compare C_6 shouldBe 1
    C_6 compare C_5 shouldBe 1
    C_5 compare C_4 shouldBe 1
    C_4 compare C_3 shouldBe 1
    C_3 compare C_2 shouldBe 1
  }
}

class HandSpec extends AnyFlatSpec with Matchers {
  behavior of "Hand"

  it should "be constructable from string" in {
    Hand("AKQJT98765432") shouldBe Hand(List(C_A, C_K, C_Q, C_J, C_T, C_9, C_8, C_7, C_6, C_5, C_4, C_3, C_2))
  }

  it should "have a defined order" in {
    Hand("AAAAA") compare Hand("AA8AA") shouldBe 1
    Hand("AA8AA") compare Hand("23332") shouldBe 1
    Hand("23332") compare Hand("TTT98") shouldBe 1
    Hand("TTT98") compare Hand("23432") shouldBe 1
    Hand("23432") compare Hand("A23A4") shouldBe 1
    Hand("A23A4") compare Hand("23456") shouldBe 1

    Hand("AAAAA") compare Hand("KKKKK") shouldBe 1
    Hand("AAAA8") compare Hand("AAAA7") shouldBe 1
  }
}

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
