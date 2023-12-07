package day06.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

sealed trait Card {
  val strength: Int
  def isStrongerThan(other: Card): Boolean = ???
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
    C_A isStrongerThan C_K shouldBe true
    C_K isStrongerThan C_Q shouldBe true
    C_Q isStrongerThan C_J shouldBe true
    C_J isStrongerThan C_T shouldBe true
    C_T isStrongerThan C_9 shouldBe true
    C_9 isStrongerThan C_8 shouldBe true
    C_8 isStrongerThan C_7 shouldBe true
    C_7 isStrongerThan C_6 shouldBe true
    C_6 isStrongerThan C_5 shouldBe true
    C_5 isStrongerThan C_4 shouldBe true
    C_4 isStrongerThan C_3 shouldBe true
    C_3 isStrongerThan C_2 shouldBe true
  }
}


class HandParserSpec extends AnyFlatSpec with Matchers {

}
