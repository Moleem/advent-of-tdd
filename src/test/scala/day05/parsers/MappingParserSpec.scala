package day05.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ContentParser

case class Mappings(
                   seeds: List[Int],
                   seedToSoilMap : Map[Int, Int],
                   soilToFertilizerMap : Map[Int, Int],
                   fertilizerToWaterMap : Map[Int, Int],
                   waterToLightMap : Map[Int, Int],
                   lightToTemperatureMap : Map[Int, Int],
                   temperatureToHumidityMap : Map[Int, Int],
                   humidityToLocationMap : Map[Int, Int]
                   )

object MappingParser extends ContentParser[Mappings] {

  private def parseSeedsLine(contentLine: String): List[Int] =
    contentLine
      .split(":")(1)
      .trim
      .split(" ")
      .map(_.trim.toInt)
      .toList


  private def parseMappingBlock(contentBlock: String): Map[Int, Int] =
    contentBlock
      .split("\n")
      .tail
      .map { line =>
        val nums = line.split(" ").map(_.trim.toInt)
        (nums(0), nums(1), nums(2))
      }
      .flatMap { case (mappingValuesStart, mappingKeysStart, mappingLength) =>
        val keys = (mappingKeysStart until mappingKeysStart + mappingLength).toList
        val values = (mappingValuesStart until mappingValuesStart + mappingLength).toList
        keys.zip(values)
      }.toMap
      .withDefault(x => x)

  override def parse(content: String): Mappings = {
    val contentBlocks = content.split("\n\n")

    Mappings(
      parseSeedsLine(contentBlocks(0)),
      parseMappingBlock(contentBlocks(1)),
      parseMappingBlock(contentBlocks(2)),
      parseMappingBlock(contentBlocks(3)),
      parseMappingBlock(contentBlocks(4)),
      parseMappingBlock(contentBlocks(5)),
      parseMappingBlock(contentBlocks(6)),
      parseMappingBlock(contentBlocks(7))
    )
  }
}

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

  it should "correctly parse seed to soil map [0:49]" in {
    val mappings = MappingParser.parse(mappingInput).seedToSoilMap

    mappings(0) shouldBe 0
    mappings(49) shouldBe 49
  }

  it should "correctly parse seed to soil map [50:50+48[" in {
    val mappings = MappingParser.parse(mappingInput).seedToSoilMap

    mappings(50) shouldBe 52
    mappings(97) shouldBe 99
  }

  it should "correctly parse seed to soil map [98:98+2[" in {
    val mappings = MappingParser.parse(mappingInput).seedToSoilMap

    mappings(98) shouldBe 50
    mappings(99) shouldBe 51
  }

  it should "correctly parse soil to fertilizer map [15:15+37[" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.soilToFertilizerMap(15) shouldBe 0
    mappings.soilToFertilizerMap(15+37-1) shouldBe 0+37-1
  }

  it should "correctly parse fertilizer to water map [53:53+8[" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.fertilizerToWaterMap(53) shouldBe 49
    mappings.fertilizerToWaterMap(53+8-1) shouldBe 49+8-1
  }

  it should "correctly parse water to light map [18:18+7[" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.waterToLightMap(18) shouldBe 88
    mappings.waterToLightMap(18+7-1) shouldBe 88+7-1
  }

  it should "correctly parse light to temperature map [77:77+23[" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.lightToTemperatureMap(77) shouldBe 45
    mappings.lightToTemperatureMap(77+23-1) shouldBe 45+23-1
  }

  it should "correctly parse temperature to humidity map [69:69+1[" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.temperatureToHumidityMap(69) shouldBe 0
    mappings.temperatureToHumidityMap(69+1-1) shouldBe 0+1-1
  }

  it should "correctly parse humidity to location map [56:56+37[" in {
    val mappings = MappingParser.parse(mappingInput)

    mappings.humidityToLocationMap(56) shouldBe 60
    mappings.humidityToLocationMap(56+37-1) shouldBe 60+37-1
  }

}
