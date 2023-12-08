package day06.solvers

import day06.model.RaceData
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SumRecordBreakingOptionsSpec extends AnyFlatSpec with Matchers{

  it should "be able to count how many ways can we break records" in {
    val input = List(RaceData(length = 7, recordDistance = 9))

    SumRecordBreakingOptions.solve(input) shouldBe 4
  }

  it should "be able to sum how many ways can we break records in all races" in {
    val input = List(
      RaceData(7, 9),
      RaceData(15, 40),
      RaceData(30, 200)
    )

    SumRecordBreakingOptions.solve(input) shouldBe 288 // 4 * 8 * 9
  }
}
