package day07.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Card._

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
