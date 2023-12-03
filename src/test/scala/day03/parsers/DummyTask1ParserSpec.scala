package day03.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import utils.ContentParser

import scala.annotation.tailrec

object SymbolAwareEngineSchemaParser extends ContentParser[Seq[Int]] {

    @tailrec
    private def getNumbers(engineVector: List[Char], digitsAccumulator: Option[Int], numsAccumulator: List[Int]): Seq[Int] = {
    (engineVector, digitsAccumulator) match {
      case (Nil, None) =>
        numsAccumulator
      case (Nil, Some(num)) =>
        num :: numsAccumulator
      case (head :: tail, None) if !head.isDigit =>
        getNumbers(tail, digitsAccumulator, numsAccumulator)
      case (head :: tail, None) if head.isDigit =>
        getNumbers(tail, Some(Character.getNumericValue(head)), numsAccumulator)
      case (head :: tail, Some(num)) if !head.isDigit =>
        getNumbers(tail, None, num :: numsAccumulator)
      case (head :: tail, Some(num)) if head.isDigit =>
        getNumbers(tail, Some(num * 10 + Character.getNumericValue(head)), numsAccumulator)
    }
  }

  override def parse(content: String): Seq[Int] = {
    val engineMatrix: Seq[Seq[Char]] = content.split("\n").toSeq.map(_.toSeq)

    val engineVector: List[Char] = engineMatrix.map(_.mkString).mkString(".").toList

    val res = getNumbers(engineVector, None, List.empty)
    res

//    val rowCount: Int = engineMatrix.length
//    val colCount: Int = engineMatrix.head.length

//    val symbolMask: Seq[Seq[Boolean]] =
//      (0 until rowCount).map(row =>
//        (0 until colCount).map { col =>
//          def isSymbol(c: Char): Boolean = !(c.isDigit || c == '.')
//
//          isSymbol(engineMatrix(row)(col))
//        }.toSeq
//      ).toSeq

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

