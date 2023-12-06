package solvers

import day06.model.RaceData
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object SumRecordBreakingOptions extends ProblemSolver[List[RaceData], Long] {
  override def solve(input: List[RaceData]): Long =
    (0 to input(0).length).count( pressTime =>
      pressTime * (input(0).length-pressTime) < input(0).recordDistance
    )
}

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
