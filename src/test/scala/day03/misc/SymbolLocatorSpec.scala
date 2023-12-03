package day03.misc

import day03.model.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SymbolLocatorSpec extends AnyFlatSpec with Matchers {

  behavior of "SymbolLocator"

  it should "locate a single symbol in a single line" in {
    val input = "*"
    SymbolLocator.locateSymbols(input) shouldBe Set(Point(0, 0))
  }

  it should "locate a single nested symbol in a single line" in {
    val input = ".#5"
    SymbolLocator.locateSymbols(input) shouldBe Set(Point(0, 1))
  }

  it should "locate multiple nested symbols in a single line" in {
    val input = ".$5!."
    SymbolLocator.locateSymbols(input) shouldBe Set(Point(0, 1), Point(0, 3))
  }

  it should "locate multiple nested symbols in multiple lines" in {
    val input = ".*5*.\n....*"
    SymbolLocator.locateSymbols(input) shouldBe Set(Point(0, 1), Point(0, 3), Point(1, 4))
  }
}
