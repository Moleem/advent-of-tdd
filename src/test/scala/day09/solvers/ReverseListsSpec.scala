package day09.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReverseListsSpec extends AnyFlatSpec with Matchers {

  behavior of ReverseLists.getClass.getName

  it should "correctly revert numbers" in {
    ReverseLists.solve(List(
      List(0, 0, 0),
      List(1, 2, 3),
      List(-5, 0, +10)
    )) shouldBe List(
      List(0, 0, 0),
      List(3, 2, 1),
      List(+10, 0, -5)
    )
  }

}
