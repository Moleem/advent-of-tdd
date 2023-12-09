package day09.parsers

import utils.ContentParser

object OasisReportParser extends ContentParser[List[List[Int]]] {

  override def parse(content: String): List[List[Int]] =
    content
      .split("\n")
      .toList
      .map(_.split(" ").toList.map(_.toInt))

}
