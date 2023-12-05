package day05.solvers

import day05.model.{MappingRange, Mappings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object FindLowestLocationWithSeedRanges extends ProblemSolver[Mappings, Long] {
  private def findIn(mappingRanges: List[MappingRange])(i: Long): Long =
    i + mappingRanges
      .find(range => i >= range.from && i <= range.to)
      .map(_.delta)
      .getOrElse(0L)

  override def solve(input: Mappings): Long = {
    input
      .seeds
      .foldLeft(List.empty[List[Long]]){ case (accumulator, num) =>
        if (accumulator.isEmpty) {
          List(List(num))
        } else if (accumulator.lastOption.exists(_.length == 1)) {
          accumulator.init :+ (accumulator.last.head to accumulator.last.head+num).toList
        } else {
          accumulator.init :+ List(num)
        }
      }.flatten
      .map(findIn(input.seedToSoilMap))
      .map(findIn(input.soilToFertilizerMap))
      .map(findIn(input.fertilizerToWaterMap))
      .map(findIn(input.waterToLightMap))
      .map(findIn(input.lightToTemperatureMap))
      .map(findIn(input.temperatureToHumidityMap))
      .map(findIn(input.humidityToLocationMap))
      .min
  }
}

class FindLowestLocationWithSeedRangesSpec extends AnyFlatSpec with Matchers {

  behavior of "FindLowestLocationWithSeedRanges"

  it should "find minimum location correctly if no mapping is defined" in {
    val mappings = Mappings(
      List(1, 1),
      List.empty,
      List.empty,
      List.empty,
      List.empty,
      List.empty,
      List.empty,
      List.empty
    )

    FindLowestLocationWithSeedRanges.solve(mappings) shouldBe 1
  }

  it should "find location correctly if everything is mapped to itself explicitly" in {
    val mappings = Mappings(
      List(1, 1),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0))
    )

    FindLowestLocationWithSeedRanges.solve(mappings) shouldBe 1
  }

  it should "find location correctly through changing mapping" in {
    val mappings = Mappings(
      List(1, 1),
      List(MappingRange(1, 1, +1)),
      List(MappingRange(2, 2, +1)),
      List(MappingRange(3, 3, +1)),
      List(MappingRange(4, 4, +1)),
      List(MappingRange(5, 5, +1)),
      List(MappingRange(6, 6, +1)),
      List(MappingRange(7, 7, +1))
    )

    FindLowestLocationWithSeedRanges.solve(mappings) shouldBe 8
  }

  it should "find minimum relevant location correctly" in {
    val mappings = Mappings(
      List(0, 2),
      List(MappingRange(1, 1, +1)),
      List(MappingRange(2, 2, +1)),
      List(MappingRange(3, 3, +1)),
      List(MappingRange(4, 4, +1)),
      List(MappingRange(5, 5, +1)),
      List(MappingRange(6, 6, +1)),
      List(MappingRange(7, 7, +1))
    )

    FindLowestLocationWithSeedRanges.solve(mappings) shouldBe 0
  }

  it should "find minimum relevant location (from range) correctly" in {
    val mappings = Mappings(
      List(1, 2),
      List(MappingRange( 1,  1, +10)),
      List(MappingRange(11, 11, +10)),
      List(MappingRange(21, 21, +10)),
      List(MappingRange(31, 31, +10)),
      List(MappingRange(41, 41, +10)),
      List(MappingRange(51, 51, +10)),
      List(MappingRange(61, 61, +10))
    )

    FindLowestLocationWithSeedRanges.solve(mappings) shouldBe 2
  }

  it should "ignore global minimum, if there it's seed is not interesting to us" in {
    val mappings = Mappings(
      List(1, 1),
      List(MappingRange(0, 0, +0), MappingRange(1, 1, +1)),
      List(MappingRange(0, 0, +0), MappingRange(2, 2, +1)),
      List(MappingRange(0, 0, +0), MappingRange(3, 3, +1)),
      List(MappingRange(0, 0, +0), MappingRange(4, 4, +1)),
      List(MappingRange(0, 0, +0), MappingRange(5, 5, +1)),
      List(MappingRange(0, 0, +0), MappingRange(6, 6, +1)),
      List(MappingRange(0, 0, +0), MappingRange(7, 7, +1))
    )

    FindLowestLocationWithSeedRanges.solve(mappings) shouldBe 8
  }

}
