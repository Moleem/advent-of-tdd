package day03.misc

import day03.model
import day03.model.{Point, Region}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumberLocatorSpec extends AnyFlatSpec with Matchers {

  behavior of "NumberLocator"

  it should "locate a single digit number in a single line" in {
    val input = "5"
    NumberLocator.locateNumbers(input) shouldBe Set(model.NumberRecord(5, Region(Point(0, 0), Point(0, 0))))
  }

  it should "locate a single digit number in a single line even if nested" in {
    val input = "x5x"
    NumberLocator.locateNumbers(input) shouldBe Set(model.NumberRecord(5, Region(Point(0, 1), Point(0, 1))))
  }

  it should "locate a multi digit number in a single line" in {
    val input = "25"
    NumberLocator.locateNumbers(input) shouldBe Set(model.NumberRecord(25, Region(Point(0, 0), Point(0, 1))))
  }

  it should "locate a multi digit number in a single line even if nested" in {
    val input = "x25x"
    NumberLocator.locateNumbers(input) shouldBe Set(model.NumberRecord(25, Region(Point(0, 1), Point(0, 2))))
  }

  it should "locate multiple numbers in a single line" in {
    val input = "x25x5x"
    NumberLocator.locateNumbers(input) shouldBe Set(
      model.NumberRecord(25, Region(Point(0, 1), Point(0, 2))),
      model.NumberRecord(5, Region(Point(0, 4), Point(0, 4)))
    )
  }

  it should "locate multiple numbers in multiple lines" in {
    val input = "x25x5x\nx4x28x"
    NumberLocator.locateNumbers(input) shouldBe Set(
      model.NumberRecord(25, Region(Point(0, 1), Point(0, 2))),
      model.NumberRecord(5, Region(Point(0, 4), Point(0, 4))),
      model.NumberRecord(4, Region(Point(1, 1), Point(1, 1))),
      model.NumberRecord(28, Region(Point(1, 3), Point(1, 4)))
    )
  }
}
