package day02.solvers

import day02.model.GameRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SumPossibleGameIdsSpec extends AnyFlatSpec with Matchers {

  behavior of "SumPossibleGameIds"

  it should "identify possible game record (on threshold)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 12),
          Map("green" -> 13),
          Map("blue" -> 14)
        )
      )
    )

    SumPossibleGameIds.solve(input) shouldBe 1
  }

  it should "identify possible game record (below threshold)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 11),
          Map("green" -> 12),
          Map("blue" -> 13)
        )
      )
    )

    SumPossibleGameIds.solve(input) shouldBe 1
  }

  it should "identify impossible game record (red above threshold)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 13),
          Map("green" -> 1),
          Map("blue" -> 1)
        )
      )
    )

    SumPossibleGameIds.solve(input) shouldBe 0
  }

  it should "identify impossible game record (green above threshold)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 1),
          Map("green" -> 14),
          Map("blue" -> 1)
        )
      )
    )

    SumPossibleGameIds.solve(input) shouldBe 0
  }

  it should "identify impossible game record (blue above threshold)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 1),
          Map("green" -> 1),
          Map("blue" -> 15)
        )
      )
    )

    SumPossibleGameIds.solve(input) shouldBe 0
  }

  it should "sum possible game ids (simple)" in {
    val input = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 1),
          Map("green" -> 1),
          Map("blue" -> 1)
        )
      ),
      GameRecord(
        id = 2,
        rounds = Seq(
          Map("red" -> 100),
          Map("green" -> 100),
          Map("blue" -> 100)
        )
      ),
      GameRecord(
        id = 3,
        rounds = Seq(
          Map("red" -> 1),
          Map("green" -> 1),
          Map("blue" -> 1)
        )
      )
    )

    SumPossibleGameIds.solve(input) shouldBe 4 // 1 + 3
  }


}

