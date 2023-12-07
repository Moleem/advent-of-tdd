package day07.model

import JokerAwareCard._

case class JokerAwareHand(cards: List[JokerAwareCard]) extends Ordered[JokerAwareHand] {
  private val countsWithoutJokers: List[Int] =
    cards.filterNot(_ == C_J).groupBy(_.strength).values.map(_.size).toList.sorted.reverse
  private val jokerCount: Int = cards.count(_ == C_J)

  private val handRanks = Map(
    List(5) ->   7,
    List(4, 1) -> 6,
    List(3, 2) -> 5,
    List(3, 1, 1) -> 4,
    List(2, 2, 1) -> 3,
    List(2, 1, 1, 1) -> 2,
    List(1, 1, 1, 1, 1) -> 1
  )

  def compare(other: JokerAwareHand): Int = {
    val thisCountsWithJoker =
      if (this.countsWithoutJokers.isEmpty) List(5)
      else this.countsWithoutJokers.head + this.jokerCount :: this.countsWithoutJokers.tail
    val otherCountsWithJoker =
      if (other.countsWithoutJokers.isEmpty) List(5)
      else other.countsWithoutJokers.head + other.jokerCount :: other.countsWithoutJokers.tail

    val handStrengthComparison = Integer.compare(handRanks(thisCountsWithJoker), handRanks(otherCountsWithJoker))
    if (handStrengthComparison != 0) handStrengthComparison
    else
      this.cards.zip(other.cards)
        .map { case (thisCard, otherCard) => thisCard compare otherCard }
        .find(_ != 0)
        .getOrElse(0)
  }
}

object JokerAwareHand {
  def apply(s: String): JokerAwareHand =
    JokerAwareHand(
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
