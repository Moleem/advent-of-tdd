package day05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ContentParser


case class Range(start: Long, end: Long)
case class Modifier(range: Range, delta: Long)

case class Content(relevantInitialRanges: Set[Range], modifiers: List[List[Modifier]])

object RangeParser extends ContentParser[Content] {
  override def parse(content: String): Content = ???
}



class NiceTrySpec extends AnyFlatSpec with Matchers {

  behavior of "RangeParser"

  it should "correctly read the initial content into a Content object" in {
    val input =
      """seeds: 79 14 55 13
        |
        |some name map:
        |50 98 2
        |52 50 48
        |
        |some other name map:
        |0 15 37
        |37 52 2
        |39 0 15""".stripMargin

    RangeParser.parse(input) shouldBe Content(
      relevantInitialRanges = Set(
        Range(79, 92),
        Range(55, 67)
      ),
      modifiers = List(
        List(
          Modifier(Range(98, 99), -48),
          Modifier(Range(50, 97), +2)
        ),
        List(
          Modifier(Range(15, 51), -15),
          Modifier(Range(52, 53), +15),
          Modifier(Range( 0, 14), +39)
        )
      )
    )

  }

}
