package day05.parsers

import day05.model.Mappings
import utils.ContentParser

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
