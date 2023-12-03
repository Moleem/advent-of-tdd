package day03.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import utils.ContentParser

object SymbolAwareEngineSchemaParser extends ContentParser[Seq[Int]] {

  override def parse(content: String): Seq[Int] = {
    ???
  }

}

class SymbolAwareEngineSchemaParserSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "SymbolAwareEngineSchemaParser"

  it should "not identify a number without a symbol next to it" in {
    val input =
      """...
        |.1.
        |...""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq.empty
  }

  it should "identify a number with a symbol next to it (top left)" in {
    val input =
      """#..
        |.1.
        |...""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(1)
  }

  it should "identify a number with a symbol next to it (top)" in {
    val input =
      """.#.
        |.1.
        |...""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(1)
  }

  it should "identify a number with a symbol next to it (top right)" in {
    val input =
      """..#
        |.1.
        |...""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(1)
  }

  it should "identify a number with a symbol next to it (right)" in {
    val input =
      """...
        |.1#
        |...""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(1)
  }

  it should "identify a number with a symbol next to it (bottom right)" in {
    val input =
      """...
        |.1.
        |..#""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(1)
  }

  it should "identify a number with a symbol next to it (bottom)" in {
    val input =
      """...
        |.1.
        |.#.""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(1)
  }

  it should "identify a number with a symbol next to it (bottom left)" in {
    val input =
      """...
        |.1.
        |#..""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(1)
  }

  it should "identify a number with a symbol next to it (left)" in {
    val input =
      """...
        |#1.
        |...""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(1)
  }

  it should "not identify a multi-digit number without a symbol next to it" in {
    val input =
      """....
        |.25.
        |....""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq.empty
  }

  it should "identify a multi-digit number with a symbol next to it (top left)" in {
    val input =
      """#...
        |.25.
        |....""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

  it should "identify a multi-digit number with a symbol next to it (top1)" in {
    val input =
      """.#..
        |.25.
        |....""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

  it should "identify a multi-digit number with a symbol next to it (top2)" in {
    val input =
      """..#.
        |.25.
        |....""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

  it should "identify a multi-digit number with a symbol next to it (top right)" in {
    val input =
      """...#
        |.25.
        |....""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

  it should "identify a multi-digit number with a symbol next to it (right)" in {
    val input =
      """....
        |.25#
        |....""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

  it should "identify a multi-digit number with a symbol next to it (bottom right)" in {
    val input =
      """....
        |.25.
        |...#""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

  it should "identify a multi-digit number with a symbol next to it (bottom2)" in {
    val input =
      """....
        |.25.
        |..#.""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

  it should "identify a multi-digit number with a symbol next to it (bottom1)" in {
    val input =
      """....
        |.25.
        |.#..""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

  it should "identify a multi-digit number with a symbol next to it (bottom left)" in {
    val input =
      """....
        |.25.
        |#...""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

  it should "identify a multi-digit number with a symbol next to it (left)" in {
    val input =
      """....
        |#25.
        |....""".stripMargin

    SymbolAwareEngineSchemaParser.parse(input) shouldBe Seq(25)
  }

}

