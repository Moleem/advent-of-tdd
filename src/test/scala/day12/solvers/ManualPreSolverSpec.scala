package day12.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ManualPreSolverSpec extends AnyFlatSpec with Matchers {

  behavior of ManualPresolver.getClass.getName

  it should "find definite match if all should be colored" in {
    val input =          List(("_____", List(5)))
    val expectedOutput = List(("XXXXX", List(5)))

    SpringErrorMatcher.solve(input) shouldBe expectedOutput
  }

}
