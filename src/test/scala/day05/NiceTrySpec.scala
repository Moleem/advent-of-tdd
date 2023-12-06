package day05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ContentParser


case class Range(start: Long, end: Long) {
  def contains(n: Long): Boolean = start <= n && n <= end
  def contains(other: Range): Boolean = this.contains(other.start) && this.contains(other.end)
}
case class Modifier(modifierRange: Range, delta: Long) {
  def modify(rangeToBeModified: Range): Set[Range] = {
    if (modifierRange.contains(rangeToBeModified.start) && modifierRange.contains(rangeToBeModified.end))
      Set(Range(rangeToBeModified.start + delta, rangeToBeModified.end + delta))
    else if (!rangeToBeModified.contains(modifierRange.start) && !rangeToBeModified.contains(modifierRange.end))
      Set(rangeToBeModified)
    else
      Set.empty
  }
}

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

  behavior of "Range"

  it should "be able to tell if a number is in the range" in {
    val range = Range(0, 5)

    range.contains(3) shouldBe true
  }

  it should "be able to tell if a number is in the range (start inclusive)" in {
    val range = Range(0, 5)

    range.contains(0) shouldBe true
  }

  it should "be able to tell if a number is in the range (end inclusive)" in {
    val range = Range(0, 5)

    range.contains(5) shouldBe true
  }

  it should "be able to tell if a number is not in the range (below)" in {
    val range = Range(0, 5)

    range.contains(-1) shouldBe false
  }

  it should "be able to tell if a number is not in the range (above)" in {
    val range = Range(0, 5)

    range.contains(6) shouldBe false
  }

  it should "be able to tell if another range is within the range (true subrange)" in {
    val range = Range(0, 5)

    range.contains(Range(1, 4)) shouldBe true
  }

  it should "be able to tell if another range is within the range (equivalence)" in {
    val range = Range(0, 5)

    range.contains(Range(0, 5)) shouldBe true
  }

  it should "be able to tell if another range is not within the range (below completely)" in {
    val range = Range(0, 5)

    range.contains(Range(-5, -1)) shouldBe false
  }

  it should "be able to tell if another range is not within the range (below starts)" in {
    val range = Range(0, 5)

    range.contains(Range(-1, 1)) shouldBe false
  }

  it should "be able to tell if another range is not within the range (above completely)" in {
    val range = Range(0, 5)

    range.contains(Range(6, 10)) shouldBe false
  }

  it should "be able to tell if another range is not within the range (above starts)" in {
    val range = Range(0, 5)

    range.contains(Range(3, 8)) shouldBe false
  }


  behavior of "Modifier"

  it should "do nothing with a range, if it doesnt overlap with the modifier" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(8, 12), +5)

    modifier.modify(range) shouldBe Set(range)
  }

  it should "modify the whole range, if it falls into the modifier's range" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(-10, 10), +5)

    modifier.modify(range) shouldBe Set(Range(5, 10))
  }

  it should "split the range and modify part of it, if only it's beginning falls into the modifier's range" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(-10, 3), +5)

    modifier.modify(range) shouldBe Set(Range(5, 8), Range(4, 5))
  }

  it should "split the range and modify part of it, if only it's end falls into the modifier's range" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(3, 8), +5)

    modifier.modify(range) shouldBe Set(Range(0, 2), Range(8, 10))
  }

  it should "split the range and modify part of it, if only it's a superset of the modifier's range" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(2, 4), +5)

    modifier.modify(range) shouldBe Set(Range(0, 1), Range(7, 9), Range(5, 5))
  }
}
