package day15.solvers

import day15.model.Lens
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class BoxSumamrizerSpec extends AnyFlatSpec with Matchers {

  behavior of BoxSummarizer.getClass.getSimpleName

  it should "summarize box contents" in {
    val input = Map(
      0 -> List(Lens("rn", 1), Lens("cm", 2)),
      3 -> List(Lens("ot", 7), Lens("ab", 5), Lens("pc", 6))
    )
    val expectedResult = 145

    BoxSummarizer.solve(input) shouldBe expectedResult
  }

}
