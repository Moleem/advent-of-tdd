package day13.parsers

import utils.ContentParser

object MirrorFinder extends ContentParser[Long] {
  override def parse(content: String): Long = {
    val matrix = content.split("\n").map(_.toList).toList

    val rows = matrix.map(_.mkString)
    val cols = matrix.transpose.map(_.mkString)

    findMirror(rows)*100 + findMirror(cols)
  }

  private def findMirror(lines: List[String]): Int =
    lines.indices.find { rowId =>
      val a = lines.take(rowId).reverse
      val b = lines.takeRight(lines.length - rowId)
      val pairs = a.zip(b)

      pairs.nonEmpty && pairs.forall { case (a, b) => a == b }
    }.getOrElse(0)
}
