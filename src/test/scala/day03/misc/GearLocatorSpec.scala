package day03.misc

import day03.model.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class GearLocatorSpec extends AnyFlatSpec with Matchers {

  behavior of "GearLocator"

  it should "locate a single gear in a single line" in {
    val input = "*"
    GearLocator.locateGears(input) shouldBe Set(Point(0, 0))
  }

  it should "locate a single nested gear in a single line" in {
    val input = ".*."
    GearLocator.locateGears(input) shouldBe Set(Point(0, 1))
  }

  it should "locate multiple nested gears in a single line" in {
    val input = ".*.*."
    GearLocator.locateGears(input) shouldBe Set(Point(0, 1), Point(0, 3))
  }

  it should "locate multiple nested gears in multiple lines" in {
    val input = ".*.*.\n....*"
    GearLocator.locateGears(input) shouldBe Set(Point(0, 1), Point(0, 3), Point(1, 4))
  }
}
