package day10.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountStepsSpec extends AnyFlatSpec with Matchers {

  behavior of CountSteps.getClass.getName

  it should "be able to count steps south" in {
    val input = List(
      List('.', 'S', '.'),
      List('.', '|', '.'),
      List('.', 'S', '.')
    )

    CountSteps.solve(input) shouldBe 2
  }

  it should "be able to count steps east" in {
    val input = List(
      List('.', '.', '.'),
      List('S', '-', 'S'),
      List('.', '.', '.')
    )

    CountSteps.solve(input) shouldBe 2
  }

}
