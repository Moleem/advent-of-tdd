package day18.parsers

import utils.ContentParser

object OvercomplicatedDecoder extends ContentParser[List[List[(Char, Int)]]] {
  override def parse(content: String): List[List[(Char, Int)]] = ???
}
