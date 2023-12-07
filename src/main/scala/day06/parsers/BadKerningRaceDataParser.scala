package day06.parsers

import day06.model.RaceData
import utils.ContentParser

object BadKerningRaceDataParser extends ContentParser[List[RaceData]] {
  override def parse(content: String): List[RaceData] = {
    val lines =
      content.split("\n").toList

    val timeLength =
      lines(0).split(":")(1).replaceAll(" ", "").toLong

    val recordDistance =
      lines(1).split(":")(1).replaceAll(" ", "").toLong

    List(RaceData(timeLength, recordDistance))
  }
}
