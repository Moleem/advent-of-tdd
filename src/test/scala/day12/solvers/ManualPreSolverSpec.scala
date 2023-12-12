package day12.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ManualPreSolverSpec extends AnyFlatSpec with Matchers {

  behavior of ManualPresolver.getClass.getName

  it should "find definite short self overlap" in {
    val input = List(("_____", List(3)))
    val expectedOutput = List(("__X__", List(3)))

    ManualPresolver.solve(input) shouldBe expectedOutput
  }

  it should "find definite long self overlap" in {
    val input = List(("_____", List(4)))
    val expectedOutput = List(("_XXX_", List(4)))

    ManualPresolver.solve(input) shouldBe expectedOutput
  }

  it should "find definite full self overlap" in {
    val input = List(("_____", List(5)))
    val expectedOutput = List(("XXXXX", List(5)))

    ManualPresolver.solve(input) shouldBe expectedOutput
  }

  it should "find definite short self overlap (multi)" in {
    val input = List(("_________", List(3, 3)))
    val expectedOutput = List(("__X___X__", List(3, 3)))

    ManualPresolver.solve(input) shouldBe expectedOutput
  }

  it should "find definite long self overlap (multi)" in {
    val input = List(("_________", List(3, 4)))
    val expectedOutput = List(("_XX__XXX_", List(3, 4)))

    ManualPresolver.solve(input) shouldBe expectedOutput
  }

  it should "find definite full self overlap (multi)" in {
    val input = List(("_____", List(2, 2)))
    val expectedOutput = List(("XXOXX", List(2, 2)))

    ManualPresolver.solve(input) shouldBe expectedOutput
  }

  it should "find definite only for parts long enough" in {
    val input = List(("_______________", List(1, 2, 3, 4)))
    val expectedOutput = List(("_______X___XX__", List(1, 2, 3, 4)))

    ManualPresolver.solve(input) shouldBe expectedOutput
  }

  it should "find definite even in complex cases" in {
    val input = List(("_________________________", List(5, 1, 4, 2, 1, 4)))
    val expectedOutput = List(("___XX______X_________X___", List(5, 1, 4, 2, 1, 4)))

    ManualPresolver.solve(input) shouldBe expectedOutput
  }

  it should "find definite full even in complex cases" in {
    val input = List(("______________________", List(5, 1, 4, 2, 1, 4)))
    val expectedOutput = List(("XXXXXOXOXXXXOXXOXOXXXX", List(5, 1, 4, 2, 1, 4)))

    ManualPresolver.solve(input) shouldBe expectedOutput
  }

}
