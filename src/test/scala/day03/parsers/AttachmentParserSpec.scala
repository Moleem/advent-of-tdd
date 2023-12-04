package day03.parsers

import day03.model.{NumberRecord, Point, Region, AttachmentRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks


class AttachmentParserSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "AttachmentParser"

  it should "find no attachments" in {
    val input =
      """*..
        |...
        |..1""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List.empty
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (top left)" in {
    val input =
      """*..
        |.1.
        |...""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(1)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (top mid)" in {
    val input =
      """.*.
        |.1.
        |...""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(1)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (top right)" in {
    val input =
      """..*
        |.1.
        |...""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(1)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (mid right)" in {
    val input =
      """...
        |.1*
        |...""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(1)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (bottom right)" in {
    val input =
      """...
        |.1.
        |..*""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(1)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (bottom mid)" in {
    val input =
      """...
        |.1.
        |.*.""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(1)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (bottom left)" in {
    val input =
      """...
        |.1.
        |*..""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(1)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a single digit (mid left)" in {
    val input =
      """...
        |*1.
        |...""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(1)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (top left)" in {
    val input =
      """*...
        |.25.
        |....""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (top mid 1)" in {
    val input =
      """.*..
        |.25.
        |....""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (top mid 2)" in {
    val input =
      """..*.
        |.25.
        |....""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (top right)" in {
    val input =
      """...*
        |.25.
        |....""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (mid right)" in {
    val input =
      """....
        |.25*
        |....""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (bottom right)" in {
    val input =
      """....
        |.25.
        |...*""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (bottom mid 2)" in {
    val input =
      """....
        |.25.
        |..*.""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (bottom mid 1)" in {
    val input =
      """....
        |.25.
        |.*..""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (bottom left)" in {
    val input =
      """....
        |.25.
        |*...""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find attachment to a multi digit number (mid left)" in {
    val input =
      """....
        |*25.
        |....""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find multiple attachments" in {
    val input =
      """25.
        |.*.
        |.52""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25, 52)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }

  it should "find multiple attachments for multiple parts" in {
    val input =
      """25..5
        |.*.*.
        |.52..""".stripMargin

    val expectedResult = Set(
      AttachmentRecord(
        '*', List(25, 52)
      ),
      AttachmentRecord(
        '*', List(5, 52)
      )
    )

    AttachmentParser.parse(input) shouldBe expectedResult
  }




}

