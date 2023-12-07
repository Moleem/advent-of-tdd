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
    val seedRanges = {
      input.seeds.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
      .zip(
        input.seeds.zipWithIndex.filter(_._2 % 2 != 0).map(_._1)
      ).map { case(start, length) => (start, start+length-1) }
    }

    var lowestEndValue = Long.MaxValue

    val seedRangesLen = seedRanges.size

    seedRanges.zipWithIndex.foreach { case (seedRange, index) =>
      println(s"Processing seed range $index/$seedRangesLen")
      val min = seedRange._1
      val max = seedRange._2

      var i = min
      while (i <= max) {
        val endValue =
          findIn(input.humidityToLocationMap)(
            findIn(input.temperatureToHumidityMap)(
              findIn(input.lightToTemperatureMap)(
                findIn(input.waterToLightMap)(
                  findIn(input.fertilizerToWaterMap)(
                    findIn(input.soilToFertilizerMap)(
                      findIn(input.seedToSoilMap)(i)
                    ))))))

        if (endValue < lowestEndValue) {
          lowestEndValue = endValue
        }

        i += 1
      }
    }

    lowestEndValue
  }
}
