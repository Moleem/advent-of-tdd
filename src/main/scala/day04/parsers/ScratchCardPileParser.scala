package day04.parsers

import day04.model.ScratchCardRecord
import utils.ContentParser

object ScratchCardPileParser extends ContentParser[List[ScratchCardRecord]] {
  override def parse(content: String): List[ScratchCardRecord] =
    content
      .split("\n")
      .map { line => line.split(":")(1) }
      .map { line => line.split("\\|") }
      .map { numListStrings =>
        numListStrings
          .map(_.split(" ")
            .map(_.trim)
            .filter(_.nonEmpty)
            .map(_.toInt)
            .toList
          )
      }
      .map { lists => ScratchCardRecord(lists(0), lists(1)) }
      .toList
}
