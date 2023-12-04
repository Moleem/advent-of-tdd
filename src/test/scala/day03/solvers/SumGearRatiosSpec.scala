package day03.solvers

import day03.model.AttachmentRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SumGearRatiosSpec extends AnyFlatSpec with Matchers {

  behavior of "SumGearRatios"

  it should "not care about less than 2 attachments" in {
    val input = Set(
      AttachmentRecord('*', List(1))
    )

    SumGearRatios.solve(input) shouldBe 0
  }

  it should "not care about more than 2 attachments" in {
    val input = Set(
      AttachmentRecord('*', List(1, 2, 3))
    )

    SumGearRatios.solve(input) shouldBe 0
  }

  it should "not care about non-gear shapes" in {
    val input = Set(
      AttachmentRecord('%', List(1, 2))
    )

    SumGearRatios.solve(input) shouldBe 0
  }

  it should "find valid gears (2 attachments)" in {
    val input = Set(
      AttachmentRecord('*', List(2, 3))
    )

    SumGearRatios.solve(input) shouldBe 6 // 2 * 3
  }

  it should "sum gear ratios" in {
    val input = Set(
      AttachmentRecord('*', List(1, 1)),
      AttachmentRecord('*', List(2, 2))
    )

    SumGearRatios.solve(input) shouldBe 5 // 1 * 1 + 2 * 2
  }

}
