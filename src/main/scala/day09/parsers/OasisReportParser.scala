package day09.parsers

import utils.ContentParser

object OasisReportParser extends ContentParser[List[List[Int]]] {

  override def parse(content: String): List[List[Int]] =
    content.split("\n").map(parseLine).toList

  private def parseLine(line: String): List[Int] =
    line.split(" ").map(_.toInt).toList
}
