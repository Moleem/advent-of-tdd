package day08.parsers

import day08.model.MovementMap
import utils.ContentParser

object MovementMapParser extends ContentParser[MovementMap] {

  private def parseDirections(line: String) =
    line.toList

  private def parseMappings(lines: List[String]) =
    lines.map { line =>
      val parts = line.split("=")
      val key = parts.head.trim
      val values = parts(1).trim.stripPrefix("(").stripSuffix(")").split(",").map(_.trim)
      val left = values(0)
      val right = values(1)

      key -> (left, right)
    }.toMap

  override def parse(content: String): MovementMap = {
    val lines = content.split("\n").toList
    val directions = parseDirections(lines.head)
    val mappings = parseMappings(lines.tail.tail)

    MovementMap(directions, mappings)
  }
}
