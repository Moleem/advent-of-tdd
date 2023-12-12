package day12.parsers

import utils.ContentParser

object SpringMapParser extends ContentParser[List[(String, List[Int])]] {
  override def parse(content: String): List[(String, List[Int])] =
    content
      .split("\n")
      .toList
      .map(line => {
        val parts = line.split(" ")
        val chars = parts(0)
        val nums = parts(1).split(",").map(_.toInt).toList

        (chars, nums)
      })
}
