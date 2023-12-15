package utils.parsers

import utils.ContentParser

object StringReader extends ContentParser[String] {
  override def parse(content: String): String = content
}
