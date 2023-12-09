package day09.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OasisRecordExtrapolationSpec extends AnyFlatSpec with Matchers {

  behavior of OasisRecordExtrapolation.getClass.getName

  it should "correctly extrapolate all zeroes" in {
    OasisRecordExtrapolation.solve(List(List(0, 0, 0))) shouldBe 0
  }

}
