package day05.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DividedDistortionScale {
  def distort(input: Long): Long = ???
}

class DividedDistortionScaleSpec extends AnyFlatSpec with Matchers {

  behavior of "DividedDistortionScale"

  it should "be applicable to the whole Long range" in {
    val scale = new DividedDistortionScale

    scale.distort(Long.MinValue) shouldBe Long.MinValue
    scale.distort(Long.MaxValue) shouldBe Long.MaxValue
  }

}
