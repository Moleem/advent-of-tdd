package day05.solvers

import day05.model.{MappingRange, Mappings}
import utils.ProblemSolver

object FindLowestLocation extends ProblemSolver[Mappings, Long] {
  private def findIn(mappingRanges: List[MappingRange])(i: Long): Long =
    i + mappingRanges
          .find( range => i >= range.from && i <= range.to)
          .map(_.delta)
          .getOrElse(0L)

  override def solve(input: Mappings): Long = {
    input
      .seeds
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
