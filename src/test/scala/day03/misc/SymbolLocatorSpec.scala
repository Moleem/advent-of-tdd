package day03.misc

import day03.model.{Point, SymbolRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SymbolLocatorSpec extends AnyFlatSpec with Matchers {

  behavior of "SymbolLocator"

  it should "locate no symbols" in {
    val input = ".5."
    SymbolLocator.locateSymbols(input) shouldBe Set.empty
  }

  it should "locate a single symbol in a single line" in {
    val input = "*"
    SymbolLocator.locateSymbols(input) shouldBe Set(
      SymbolRecord('*', Point(0, 0))
    )
  }

  it should "locate a single nested symbol in a single line" in {
    val input = ".#5"
    SymbolLocator.locateSymbols(input) shouldBe Set(
      SymbolRecord('#', Point(0, 1))
    )
  }

  it should "locate multiple nested symbols in a single line" in {
    val input = ".$5!."
    SymbolLocator.locateSymbols(input) shouldBe Set(
      SymbolRecord('$', Point(0, 1)),
      SymbolRecord('!', Point(0, 3))
    )
  }

  it should "locate multiple nested symbols in multiple lines" in {
    val input = ".*5*.\n....*"
    SymbolLocator.locateSymbols(input) shouldBe Set(
      SymbolRecord('*', Point(0, 1)),
      SymbolRecord('*', Point(0, 3)),
      SymbolRecord('*', Point(1, 4))
    )
  }
}
