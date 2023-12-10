package day09.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OasisRecordExtrapolationSpec extends AnyFlatSpec with Matchers {

  behavior of OasisRecordExtrapolation.getClass.getName

  it should "correctly extrapolate all zeroes" in {
    OasisRecordExtrapolation.solve(List(List(0, 0, 0))) shouldBe List(0)
  }

  it should "correctly extrapolate in one step" in {
    OasisRecordExtrapolation.solve(List(List(0, 1, 2))) shouldBe List(3)
  }

  it should "correctly extrapolate in multiple steps" in {
    OasisRecordExtrapolation.solve(List(List(0, 1, 3, 6, 10))) shouldBe List(15)
  }

  it should "correctly extrapolate multiple lines" in {
    OasisRecordExtrapolation.solve(List(
      List(0, 0, 0),
      List(1, 1, 1),
      List(0, 1, 2),
      List(0, 1, 3),
    )) shouldBe List(
      0, 1, 3, 6
    )
  }
}
