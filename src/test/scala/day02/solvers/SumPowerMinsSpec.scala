package day02.solvers

import day02.model.GameRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SumPowerMinsSpec extends AnyFlatSpec with Matchers {

  behavior of "SumPowerMins"

  it should "identify min counts (red)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 2),
          Map("red" -> 1),
          Map("red" -> 1)
        )
      )
    )

    SumPowerMins.solve(input) shouldBe 2 // Max(2, 1, 1)
  }

  it should "identify min counts (green)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("green" -> 2),
          Map("green" -> 1),
          Map("green" -> 1)
        )
      )
    )

    SumPowerMins.solve(input) shouldBe 2 // Max(2, 1, 1)
  }

  it should "identify min counts (blue)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("blue" -> 2),
          Map("blue" -> 1),
          Map("blue" -> 1)
        )
      )
    )

    SumPowerMins.solve(input) shouldBe 2 // Max(2, 1, 1)
  }

  it should "calculate power of mins (all)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 2),
          Map("green" -> 2),
          Map("blue" -> 2)
        )
      )
    )

    SumPowerMins.solve(input) shouldBe 8 // 2 * 2 * 2
  }

  it should "sum power of mins (simple)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 2),
          Map("green" -> 2),
          Map("blue" -> 2)
        )
      ),
      GameRecord(
        id = 2,
        rounds = Seq(
          Map("red" -> 2),
          Map("green" -> 2),
          Map("blue" -> 2)
        )
      )
    )

    SumPowerMins.solve(input) shouldBe 16 // (2 * 2 * 2) + (2 * 2 * 2)
  }

  it should "sum power of mins (example)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("blue" -> 3, "red" -> 4),
          Map("red" -> 1, "green" -> 2, "blue" -> 6),
          Map("green" -> 2)
        )
      ),
      GameRecord(
        id = 2,
        rounds = Seq(
          Map("blue" -> 1, "green" -> 2),
          Map("green" -> 3, "blue" -> 4, "red" -> 1),
          Map("green" -> 1, "blue" -> 1)
        )
      ),
      GameRecord(
        id = 3,
        rounds = Seq(
          Map("green" -> 8, "blue" -> 6, "red" -> 20),
          Map("blue" -> 5, "red" -> 4, "green" -> 13),
          Map("green" -> 5, "red" -> 1)
        )
      ),
      GameRecord(
        id = 4,
        rounds = Seq(
          Map("green" -> 1, "red" -> 3, "blue" -> 6),
          Map("green" -> 3, "red" -> 6),
          Map("green" -> 3, "blue" -> 15, "red" -> 14)
        )
      ),
      GameRecord(
        id = 5,
        rounds = Seq(
          Map("red" -> 6, "blue" -> 1, "green" -> 3),
          Map("blue" -> 2, "red" -> 1, "green" -> 2)
        )
      )
    )

    SumPowerMins.solve(input) shouldBe 2286 // 48 + 12 + 1560 + 630 + 36
  }

}

