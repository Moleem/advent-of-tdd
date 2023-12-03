package day03.parsers

import day03.model.{NumberRecord, Point, Region, SymbolRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import utils.ContentParser


object SymbolNeighborsParser extends ContentParser[Set[SymbolRecord]] {

  override def parse(content: String): Set[SymbolRecord] = ???
}

class GearPartsParserSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "SymbolNeighborsParser"

  it should "find attachment to a single digit (top left)" in {
    val input =
      """*..
        |.1.
        |...""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(0, 0),
        Set(NumberRecord(1, Region(Point(1, 1), Point(1, 1))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (top mid)" in {
    val input =
      """.*.
        |.1.
        |...""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(0, 1),
        Set(NumberRecord(1, Region(Point(1, 1), Point(1, 1))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (top right)" in {
    val input =
      """..*
        |.1.
        |...""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(0, 2),
        Set(NumberRecord(1, Region(Point(1, 1), Point(1, 1))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (mid right)" in {
    val input =
      """...
        |.1*
        |...""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(1, 2),
        Set(NumberRecord(1, Region(Point(1, 1), Point(1, 1))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (bottom right)" in {
    val input =
      """...
        |.1.
        |..*""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(2, 2),
        Set(NumberRecord(1, Region(Point(1, 1), Point(1, 1))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (bottom mid)" in {
    val input =
      """...
        |.1.
        |.*.""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(2, 1),
        Set(NumberRecord(1, Region(Point(1, 1), Point(1, 1))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (bottom left)" in {
    val input =
      """...
        |.1.
        |*..""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(2, 0),
        Set(NumberRecord(1, Region(Point(1, 1), Point(1, 1))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (mid left)" in {
    val input =
      """...
        |*1.
        |...""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(1, 0),
        Set(NumberRecord(1, Region(Point(1, 1), Point(1, 1))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (top left)" in {
    val input =
      """*...
        |.25.
        |....""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(0, 0),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (top mid 1)" in {
    val input =
      """.*..
        |.25.
        |....""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(0, 1),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (top mid 2)" in {
    val input =
      """..*.
        |.25.
        |....""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(0, 2),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (top right)" in {
    val input =
      """...*
        |.25.
        |....""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(0, 3),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (mid right)" in {
    val input =
      """....
        |.25*
        |....""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(1, 3),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (bottom right)" in {
    val input =
      """....
        |.25.
        |...*""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(2, 3),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (bottom mid 2)" in {
    val input =
      """....
        |.25.
        |..*.""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(2, 2),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (bottom mid 1)" in {
    val input =
      """....
        |.25.
        |.*..""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(2, 1),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (bottom left)" in {
    val input =
      """....
        |.25.
        |*...""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(2, 0),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (mid left)" in {
    val input =
      """....
        |*25.
        |....""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(1, 0),
        Set(NumberRecord(25, Region(Point(1, 1), Point(1, 2))))
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find multiple attachments" in {
    val input =
      """25.
        |.*.
        |.52""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(1, 1),
        Set(
          NumberRecord(25, Region(Point(0, 0), Point(0, 1))),
          NumberRecord(52, Region(Point(2, 1), Point(2, 2)))
        )
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }

  it should "find multiple attachments for multiple parts" in {
    val input =
      """25..5
        |.*.*.
        |.52..""".stripMargin

    val expectedResult = Set(
      SymbolRecord(
        '*', Point(1, 1),
        Set(
          NumberRecord(25, Region(Point(0, 0), Point(0, 1))),
          NumberRecord(52, Region(Point(2, 1), Point(2, 2)))
        )
      ),
      SymbolRecord(
        '*', Point(1, 3),
        Set(
          NumberRecord(5, Region(Point(0, 4), Point(0, 4))),
          NumberRecord(52, Region(Point(2, 1), Point(2, 2)))
        )
      )
    )

    SymbolNeighborsParser.parse(input) shouldBe expectedResult
  }




}

