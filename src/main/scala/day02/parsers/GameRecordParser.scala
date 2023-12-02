package day02.parsers

import day02.model._
import utils.ContentParser

object GameRecordParser extends ContentParser[Seq[GameRecord]] {

  private def parseGameId(line: String): Int =
    line.split(":")(0).split(" ")(1).toInt

  private def parseRounds(line: String): Seq[Map[String, Int]] =
    line
      .split(":")(1)
      .split(";")
      .map { draw =>
        draw.split(",").map(_.trim)
          .map { draw =>
            val color = draw.split(" ")(1)
            val amount = draw.split(" ")(0).toInt
            color -> amount
          }.toMap
      }

  private def parseLine(line: String): GameRecord =
    GameRecord(parseGameId(line), parseRounds(line))

  override def parse(content: String): Seq[GameRecord] =
    content
      .split("\n")
      .map(parseLine)
}
