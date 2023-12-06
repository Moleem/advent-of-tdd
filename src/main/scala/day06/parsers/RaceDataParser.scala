package day06.parsers

import day06.model.RaceData
import utils.ContentParser

object RaceDataParser extends ContentParser[List[RaceData]] {
  override def parse(content: String): List[RaceData] = {
    val lines =
      content.split("\n").toList

    val timeLengths =
      lines(0).split(":")(1).trim.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt)

    val recordDistances =
      lines(1).split(":")(1).trim.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt)

    timeLengths.zip(recordDistances).toList.map { case (length, recordDistance) => RaceData(length, recordDistance) }
  }
}
