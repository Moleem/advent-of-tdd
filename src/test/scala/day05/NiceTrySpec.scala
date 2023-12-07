package day05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.{ContentParser, MyLittleFileReader, PrintSolution, ProblemSolver}


case class Range(start: Long, end: Long) {

  def contains(n: Long): Boolean =
    start <= n && n <= end

  def contains(other: Range): Boolean =
    this.contains(other.start) && this.contains(other.end)

}

case class ModificationResult(modifiedSubRanges: Set[Range], unmodifiedSubRanges: Set[Range]) {
  def merge(other: ModificationResult): ModificationResult = ???

}

case class Modifier(modifierRange: Range, delta: Long) {

  def modify(rangesToBeModified: Set[Range]): ModificationResult =
    rangesToBeModified.map(this.modify).foldLeft(ModificationResult(Set(), Set())) { case (prevModRes, nextModRes) =>
      ModificationResult(
        modifiedSubRanges = prevModRes.modifiedSubRanges ++ nextModRes.modifiedSubRanges,
        unmodifiedSubRanges = prevModRes.unmodifiedSubRanges ++ nextModRes.unmodifiedSubRanges
      )
    }

  def modify(rangeToBeModified: Range): ModificationResult = {
    if (modifierRange.contains(rangeToBeModified))
      ModificationResult(
        modifiedSubRanges = Set(
          Range(rangeToBeModified.start + delta, rangeToBeModified.end + delta)
        ),
        unmodifiedSubRanges = Set()
      )

    else if (modifierRange.contains(rangeToBeModified.start)) {
      ModificationResult(
        modifiedSubRanges = Set(
          Range(rangeToBeModified.start + delta, modifierRange.end + delta)
        ),
        unmodifiedSubRanges = Set(
          Range(modifierRange.end + 1, rangeToBeModified.end)
        )
      )
    } else if (modifierRange.contains(rangeToBeModified.end))
      ModificationResult(
        modifiedSubRanges = Set(
          Range(modifierRange.start + delta, rangeToBeModified.end + delta)
        ),
        unmodifiedSubRanges = Set(
          Range(rangeToBeModified.start, modifierRange.start - 1)
        )
      )
    else if (rangeToBeModified.contains(modifierRange))
      ModificationResult(
        modifiedSubRanges = Set(
          Range(modifierRange.start + delta, modifierRange.end + delta)
        ),
        unmodifiedSubRanges = Set(
          Range(rangeToBeModified.start, modifierRange.start - 1),
          Range(modifierRange.end + 1, rangeToBeModified.end)
        )
      )
    else
      ModificationResult(
        modifiedSubRanges = Set(),
        unmodifiedSubRanges = Set(rangeToBeModified)
      )
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

object MinIndexFinder extends ProblemSolver[Content, Long] {
  override def solve(input: Content): Long = {
    input.modifiers
      .foldLeft(input.relevantInitialRanges) {
        case (rangesToBeModified, modifiers) =>
          val initialModificationResult = ModificationResult(
            modifiedSubRanges = Set(),
            unmodifiedSubRanges = rangesToBeModified
          )

          val modificationResult = modifiers.foldLeft(initialModificationResult) { case (previousModificationResult, nextModifier) =>
            val newModificationResults =
              nextModifier.modify(previousModificationResult.unmodifiedSubRanges)

            ModificationResult(
              modifiedSubRanges = previousModificationResult.modifiedSubRanges ++ newModificationResults.modifiedSubRanges,
              unmodifiedSubRanges = newModificationResults.unmodifiedSubRanges
            )
          }

          modificationResult.modifiedSubRanges ++ modificationResult.unmodifiedSubRanges
      }.map(_.start)
      .min
  }
}

class NiceTrySpec extends AnyFlatSpec with Matchers {


  behavior of "RangeParser"

  it should "correctly read the initial content into a Content object" in {
    val input =
      """seeds: 79 14 55 13
        |
        |seed-to-soil map:
        |50 98 2
        |52 50 48
        |
        |soil-to-fertilizer map:
        |0 15 37
        |37 52 2
        |39 0 15
        |
        |fertilizer-to-water map:
        |49 53 8
        |0 11 42
        |42 0 7
        |57 7 4
        |
        |water-to-light map:
        |88 18 7
        |18 25 70
        |
        |light-to-temperature map:
        |45 77 23
        |81 45 19
        |68 64 13
        |
        |temperature-to-humidity map:
        |0 69 1
        |1 0 69
        |
        |humidity-to-location map:
        |60 56 37
        |56 93 4""".stripMargin

    RangeParser.parse(input) shouldBe Content(
      Set(Range(79, 92), Range(55, 67)),
      List(
        Set(
          Modifier(Range(98, 99), -48),
          Modifier(Range(50, 97), 2)),
        Set(
          Modifier(Range(15, 51), -15),
          Modifier(Range(52, 53), -15),
          Modifier(Range(0, 14), 39)),
        Set(
          Modifier(Range(53, 60), -4),
          Modifier(Range(11, 52), -11),
          Modifier(Range(0, 6), 42),
          Modifier(Range(7, 10), 50)),
        Set(
          Modifier(Range(18, 24), 70),
          Modifier(Range(25, 94), -7)),
        Set(
          Modifier(Range(77, 99), -32),
          Modifier(Range(45, 63), 36),
          Modifier(Range(64, 76), 4)),
        Set(
          Modifier(Range(69, 69), -69),
          Modifier(Range(0, 68), 1)),
        Set(
          Modifier(Range(56, 92), 4),
          Modifier(Range(93, 96), -37)
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

  it should "yield no result, if it doesnt overlap with the modifier" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(8, 12), +5)

    modifier.modify(range) shouldBe ModificationResult(
      modifiedSubRanges = Set(),
      unmodifiedSubRanges = Set(Range(0, 5))
    )
  }

  it should "modify the whole range, if it falls into the modifier's range" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(-10, 10), +5)

    modifier.modify(range) shouldBe ModificationResult(
      modifiedSubRanges = Set(Range(5, 10)),
      unmodifiedSubRanges = Set()
    )
  }

  it should "split the range and modify part of it, if only it's beginning falls into the modifier's range" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(-10, 3), +5)

    modifier.modify(range) shouldBe ModificationResult(
      modifiedSubRanges = Set(Range(5, 8)),
      unmodifiedSubRanges = Set(Range(4, 5))
    )
  }

  it should "split the range and modify part of it, if only it's end falls into the modifier's range" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(3, 8), +5)

    modifier.modify(range) shouldBe ModificationResult(
      modifiedSubRanges = Set(Range(8, 10)),
      unmodifiedSubRanges = Set(Range(0, 2))
    )
  }

  it should "split the range and modify part of it, if only it's a superset of the modifier's range" in {
    val range = Range(0, 5)
    val modifier = Modifier(Range(2, 4), +5)

    modifier.modify(range) shouldBe ModificationResult(
      modifiedSubRanges = Set(Range(7, 9)),
      unmodifiedSubRanges = Set(Range(0, 1), Range(5, 5))
    )
  }

  it should "be able to modify multiple ranges at the same time" in {
    val ranges = Set(Range(0, 5), Range(10, 15))
    val modifier = Modifier(Range(3, 13), +20)

    modifier.modify(ranges) shouldBe ModificationResult(
      modifiedSubRanges = Set(
        Range(23, 25),
        Range(30, 33)
      ), unmodifiedSubRanges = Set(
        Range(0, 2),
        Range(14, 15)
      )
    )
  }


  behavior of "ModificationResult"

  it should "be able to completely merge with an other modification result" in {
    val modificationResultA = ModificationResult(
      modifiedSubRanges = Set(Range(0, 1)),
      unmodifiedSubRanges = Set(Range(4, 5))
    )
    val modificationResultB = ModificationResult(
      modifiedSubRanges = Set(Range(2, 3)),
      unmodifiedSubRanges = Set(Range(6, 7))
    )

    val expectedModificationResult = ModificationResult(
      modifiedSubRanges = Set(
        Range(0, 1), Range(2, 3)
      ), unmodifiedSubRanges = Set(
        Range(4, 5), Range(6, 7)
      )
    )

    modificationResultA.merge(modificationResultB) shouldBe expectedModificationResult
  }

  behavior of "MinIndexFinder"

  it should "correctly apply multiple modifiers after each other" in {
    val content = Content(
      Set(Range(2, 6)),
      List(
        Set(
          Modifier(Range(3, 4), +4),
          Modifier(Range(7, 8), -4),
          Modifier(Range(2, 2), +7),
          Modifier(Range(9, 9), -7)
        )
      )
    )

    MinIndexFinder.solve(content) shouldBe 5
  }

  it should "apply modifiers on the initial range and find the lowest index of the output" in {

    val content = Content(
      Set(Range(79, 92), Range(55, 67)),
      List(
        Set(
          Modifier(Range(98, 99), -48),
          Modifier(Range(50, 97), 2)),
        Set(
          Modifier(Range(15, 51), -15),
          Modifier(Range(52, 53), -15),
          Modifier(Range(0, 14), 39)),
        Set(
          Modifier(Range(53, 60), -4),
          Modifier(Range(11, 52), -11),
          Modifier(Range(0, 6), 42),
          Modifier(Range(7, 10), 50)),
        Set(
          Modifier(Range(18, 24), 70),
          Modifier(Range(25, 94), -7)),
        Set(
          Modifier(Range(77, 99), -32),
          Modifier(Range(45, 63), 36),
          Modifier(Range(64, 76), 4)),
        Set(
          Modifier(Range(69, 69), -69),
          Modifier(Range(0, 68), 1)),
        Set(
          Modifier(Range(56, 92), 4),
          Modifier(Range(93, 96), -37)
        )
      )
    )


    MinIndexFinder.solve(content) shouldBe 46
  }

  it should "solve the second task faster than a lifetime" in {
    val content = MyLittleFileReader.readFile("/day05/input-2.txt")
    val parsedContent = RangeParser.parse(content)
    val solution = MinIndexFinder.solve(parsedContent)

    solution shouldBe 26714516
  }
}
