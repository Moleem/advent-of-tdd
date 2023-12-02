package day01.solvers

import day01.model.Calibration
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SumFirstAndLastDigitsSpec extends AnyFlatSpec with Matchers {

  behavior of "SumFirstAndLastDigits"

  it should "return the value of a single element sequence" in {
    SumFirstAndLastDigits.solve(Seq(Calibration(2, 5))) shouldBe 25
  }

  it should "return sum the values of multi element sequences" in {
    SumFirstAndLastDigits.solve(Seq(Calibration(2, 5), Calibration(2, 5))) shouldBe 50
  }

}

