package utils.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class StringReaderSpec extends AnyFlatSpec with Matchers {

  behavior of StringReader.getClass.getSimpleName

  it should "return content string as is" in {
    val input =
      """line1
        |line2
        |line3""".stripMargin

    StringReader.parse(input) shouldBe input
  }

}
