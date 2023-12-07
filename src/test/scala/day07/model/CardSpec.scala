package day07.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CardSpec extends AnyFlatSpec with Matchers {
  behavior of "Card"

  it should "have a defined order" in {
    C_A compare C_K shouldBe 1
    C_K compare C_Q shouldBe 1
    C_Q compare C_T shouldBe 1
    C_T compare C_9 shouldBe 1
    C_9 compare C_8 shouldBe 1
    C_8 compare C_7 shouldBe 1
    C_7 compare C_6 shouldBe 1
    C_6 compare C_5 shouldBe 1
    C_5 compare C_4 shouldBe 1
    C_4 compare C_3 shouldBe 1
    C_3 compare C_2 shouldBe 1
    C_2 compare C_J shouldBe 1
  }
}
