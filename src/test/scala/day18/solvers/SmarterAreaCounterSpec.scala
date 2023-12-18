package day18.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SmarterAreaCounterSpec extends AnyFlatSpec with Matchers {

  behavior of SmarterAreaCounter.getClass.getSimpleName

  it should "be able to count area for a line" in {
    val input = List(('R', 3))
    // ###
    SmarterAreaCounter.solve(input) shouldBe 3
  }

}
