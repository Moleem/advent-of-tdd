package day05.parsers

import day05.model.MappingRange
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class MappingParserSpec extends AnyFlatSpec with Matchers {

  private val mappingInput =
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

  behavior of "MappingParser"

  it should "correctly parse out seeds" in {
    MappingParser.parse(mappingInput).seeds shouldBe List(79, 14, 55, 13)
  }

  it should "correctly parse seed to soil map" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.seedToSoilMap should contain theSameElementsAs List(
      MappingRange(from = 50, to = 97, delta = + 2),
      MappingRange(from = 98, to = 99, delta = -48)
    )
  }

  it should "correctly parse soil to fertilizer map" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.soilToFertilizerMap should contain theSameElementsAs List(
      MappingRange(from =  0, to = 14, delta = +39),
      MappingRange(from = 15, to = 51, delta = -15),
      MappingRange(from = 52, to = 53, delta = -15)
    )
  }

  it should "correctly parse fertilizer to water map" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.fertilizerToWaterMap should contain theSameElementsAs List(
      MappingRange(from =  0, to =  6, delta = +42),
      MappingRange(from =  7, to = 10, delta = +50),
      MappingRange(from = 11, to = 52, delta = -11),
      MappingRange(from = 53, to = 60, delta = - 4)
    )
  }

  it should "correctly parse water to light map" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.waterToLightMap should contain theSameElementsAs List(
      MappingRange(from = 18, to = 24, delta = +70),
      MappingRange(from = 25, to = 94, delta = - 7)
    )
  }

  it should "correctly parse light to temperature map" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.lightToTemperatureMap should contain theSameElementsAs List(
      MappingRange(from = 45, to = 63, delta = +36),
      MappingRange(from = 64, to = 76, delta = + 4),
      MappingRange(from = 77, to = 99, delta = -32)
    )
  }

  it should "correctly parse temperature to humidity map" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.temperatureToHumidityMap should contain theSameElementsAs List(
      MappingRange(from =  0, to = 68, delta = + 1),
      MappingRange(from = 69, to = 69, delta = -69)
    )
  }

  it should "correctly parse humidity to location map" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.humidityToLocationMap should contain theSameElementsAs List(
      MappingRange(from = 56, to = 92, delta = + 4),
      MappingRange(from = 93, to = 96, delta = -37)
    )
  }

}
