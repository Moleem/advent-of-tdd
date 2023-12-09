package day09.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReverseListSpec extends AnyFlatSpec with Matchers {

  behavior of ReverseList.getClass.getName

  it should "correctly revert numbers" in {
    ReverseList.solve(List(-5, 0, +10)) shouldBe List(+10, 0, -5)
  }

}
