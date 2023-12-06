package solvers

import day06.model.RaceData
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object SumRecordBreakingOptions extends ProblemSolver[List[RaceData], Long] {
  override def solve(input: List[RaceData]): Long = ???
}

class SumRecordBreakingOptionsSpec extends AnyFlatSpec with Matchers{

  it should "be able to count how many ways can we break records" in {
    val input = List(RaceData(length = 7, recordDistance = 9))

    SumRecordBreakingOptions.solve(input) shouldBe 4
  }
}
