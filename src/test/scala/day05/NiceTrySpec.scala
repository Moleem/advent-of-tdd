package day05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ContentParser


case class Range(start: Long, end: Long)
case class Modifier(range: Range, delta: Long)

case class Content(relevantInitialRanges: Set[Range], modifiers: List[Set[Modifier]])

object RangeParser extends ContentParser[Content] {

  private def parseRelevantInitialRanges(line: String): Set[Range] = {
    val numbers = line.split(":")(1).split(" ").filterNot(_.isEmpty).map(_.toLong)
    val starts = numbers.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val lengths = numbers.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)

    starts.zip(lengths).map { case (start, length) =>
      Range(start, start + length - 1)
    }.toSet
  }

  private def parseModifier(line: String): Modifier = {
    val parts = line.split(" ")
    val start = parts(1).toLong
    val length = parts(2).toLong
    val end = start + length - 1
    val delta = parts(0).toLong - start

    Modifier(Range(start, end), delta)
  }

  private def parseModifiers(lines: String): Set[Modifier] = {
    val nonHeaderLines = lines.split("\n").tail

    nonHeaderLines.map(parseModifier).toSet
  }

  override def parse(content: String): Content = {
    val parts = content.split("\n\n")
    val relevantInitialRanges = parseRelevantInitialRanges(parts.head)
    val modifiers = parts.tail.map(parseModifiers).toList

    Content(relevantInitialRanges, modifiers)
  }
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
        Set(
          Modifier(Range(98, 99), -48),
          Modifier(Range(50, 97), +2)
        ),
        Set(
          Modifier(Range(15, 51), -15),
          Modifier(Range(52, 53), -15),
          Modifier(Range( 0, 14), +39)
        )
      )
    )
  }

}
