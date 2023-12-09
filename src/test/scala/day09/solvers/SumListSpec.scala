package day09.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SumListSpec extends AnyFlatSpec with Matchers {

  behavior of SumList.getClass.getName

  it should "correctly sum numbers" in {
    SumList.solve(List(-5, 0, +10)) shouldBe 5
  }

}
