package day05.solvers

import day05.model.{MappingRange, Mappings}
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
      .foldLeft(List.empty[List[Long]]) { case (accumulator, num) =>
        if (accumulator.isEmpty) {
          List(List(num))
        } else if (accumulator.lastOption.exists(_.length == 1)) {
          accumulator.init :+ (accumulator.last.head to accumulator.last.head + num).toList
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
