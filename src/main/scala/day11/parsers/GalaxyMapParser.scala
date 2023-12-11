package day11.parsers

import utils.ContentParser

object GalaxyMapParser extends ContentParser[ Map[Int, (Int, Int)]] {
  override def parse(content: String):  Map[Int, (Int, Int)] = ???
}
