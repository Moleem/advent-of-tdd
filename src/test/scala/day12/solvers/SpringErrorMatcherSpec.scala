package day12.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpringErrorMatcherSpec extends AnyFlatSpec with Matchers {

  behavior of SpringErrorMatcher.getClass.getName

  it should "return a single possible arrangement if there are no unknowns" in {
    val input = List((".#.##.###.", List(1, 2, 3)))

    SpringErrorMatcher.solve(input) shouldBe 1
  }
}
