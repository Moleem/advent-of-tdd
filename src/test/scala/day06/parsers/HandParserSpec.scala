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

case class Hand(cards: List[Card])

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


class HandParserSpec extends AnyFlatSpec with Matchers {

}
