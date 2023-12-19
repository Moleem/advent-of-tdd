package day18.parsers

import utils.ContentParser

object OvercomplicatedDecoder extends ContentParser[List[(Char, Int)]] {
  override def parse(content: String): List[(Char, Int)] =
    content.split("\n")
      .map { line =>
        line.split(" ")(2).stripPrefix("(#").stripPrefix(")")
      }.map { hexa =>
      val distance = Integer.parseInt(hexa.substring(0, 5), 16)
      val direction = hexa.charAt(5) match {
        case '0' => 'R'
        case '1' => 'D'
        case '2' => 'L'
        case '3' => 'U'
      }
      (direction, distance)
    }.toList
}
