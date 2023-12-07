package day06.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

case class Hand(cards: List[Card]) {
  def compare(other: Hand): Int = ???
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

  //Five of a kind, where all five cards have the same label: AAAAA
  //Four of a kind, where four cards have the same label and one card has a different label: AA8AA
  //Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
  //Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
  //Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
  //One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
  //High card, where all cards' labels are distinct: 23456

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

    Hand("AAAAA") compare Hand("BBBBB") shouldBe 1
    Hand("AAAA8") compare Hand("AAAA7") shouldBe 1
  }
}


class HandParserSpec extends AnyFlatSpec with Matchers {

}
