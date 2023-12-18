package day18.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SmarterAreaCounterSpec extends AnyFlatSpec with Matchers {

  behavior of SmarterAreaCounter.getClass.getSimpleName

  it should "be able to count area for a line" in {
    val input = List(('R', 3))
    // ###
    SmarterAreaCounter.solve(input) shouldBe 4
  }

  it should "be able to count area for a rectangle" in {
    val input = List(
      ('R', 3),
      ('D', 2),
      ('L', 3),
      ('U', 2),
    )
    // ####
    // ####
    // ####
    SmarterAreaCounter.solve(input) shouldBe 12
  }

}
