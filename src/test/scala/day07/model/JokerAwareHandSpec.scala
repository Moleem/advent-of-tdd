package day07.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import JokerAwareCard._

class JokerAwareHandSpec extends AnyFlatSpec with Matchers {

  behavior of "JokerAwareHand"

  it should "be constructable from string" in {
    JokerAwareHand("AKQJT98765432") shouldBe JokerAwareHand(List(C_A, C_K, C_Q, C_J, C_T, C_9, C_8, C_7, C_6, C_5, C_4, C_3, C_2))
  }

  it should "have a defined order (joker aware)" in {
    JokerAwareHand("KTJJT") compare JokerAwareHand("QQQJA") shouldBe 1
    JokerAwareHand("QQQJA") compare JokerAwareHand("T55J5") shouldBe 1
    JokerAwareHand("T55J5") compare JokerAwareHand("KK677") shouldBe 1
    JokerAwareHand("KK677") compare JokerAwareHand("32T3K") shouldBe 1
    JokerAwareHand("22222") compare JokerAwareHand("JJJJJ") shouldBe 1
  }
}
