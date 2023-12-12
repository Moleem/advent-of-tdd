package day12.parsers

import utils.ContentParser

object SpringMapParser extends ContentParser[List[(String, List[Int])]] {
  override def parse(content: String): List[(String, List[Int])] = ???
}
