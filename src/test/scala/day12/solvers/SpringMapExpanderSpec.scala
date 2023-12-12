package day12.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpringMapExpanderSpec extends AnyFlatSpec with Matchers {

  behavior of SpringMapExpander.getClass.getName

  it should "concatenate strings with ? and multiply nums" in {
    val input = List(
      (".#", List(1)),
      ("???.###", List(1, 1, 3))
    )

    val expectedOutput = List(
      (".#?.#?.#?.#?.#", List(1,1,1,1,1)),
      ("???.###????.###????.###????.###????.###", List(1,1,3,1,1,3,1,1,3,1,1,3,1,1,3))
    )

    SpringMapExpander.solve(input) shouldBe expectedOutput
  }
}
