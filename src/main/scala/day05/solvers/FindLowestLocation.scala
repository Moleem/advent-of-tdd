package day05.solvers

import day05.model.Mappings
import utils.ProblemSolver

object FindLowestLocation extends ProblemSolver[Mappings, Int] {
  override def solve(input: Mappings): Int =
    input
      .seeds
      .map(input.seedToSoilMap)
      .map(input.soilToFertilizerMap)
      .map(input.fertilizerToWaterMap)
      .map(input.waterToLightMap)
      .map(input.lightToTemperatureMap)
      .map(input.temperatureToHumidityMap)
      .map(input.humidityToLocationMap)
      .min
}
