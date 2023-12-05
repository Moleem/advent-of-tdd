package day05.parsers

import day05.model.{MappingRange, Mappings}
import utils.ContentParser

object MappingParser extends ContentParser[Mappings] {

  private def parseSeedsLine(contentLine: String): List[Long] =
    contentLine
      .split(":")(1)
      .trim
      .split(" ")
      .map(_.trim.toLong)
      .toList

  private def parseMappingBlock(contentBlock: String): List[MappingRange] = {
    contentBlock
      .split("\n")
      .tail
      .map { line =>
        val nums = line.split(" ").map(_.trim.toLong)
        (nums(0), nums(1), nums(2))
      }
      .map { case (mappingValuesStart, mappingKeysStart, mappingLength) =>
        MappingRange(mappingKeysStart, mappingKeysStart+mappingLength-1, mappingValuesStart-mappingKeysStart)
      }
      .toList
  }

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
