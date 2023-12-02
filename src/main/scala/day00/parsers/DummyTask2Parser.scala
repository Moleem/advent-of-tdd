package day00.parsers

import day00.model.DummyRecord
import utils.ContentParser

object DummyTask2Parser extends ContentParser[Seq[DummyRecord]] {

  override def parse(content: String): Seq[DummyRecord] =
    content
      .split("\n")
      .map(line => DummyRecord(line))
}
