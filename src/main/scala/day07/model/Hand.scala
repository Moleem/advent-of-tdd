package day07.model

import Card._

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
