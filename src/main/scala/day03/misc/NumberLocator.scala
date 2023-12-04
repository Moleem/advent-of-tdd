package day03.misc

import day03.model._

import scala.annotation.tailrec

object NumberLocator {
  def locateNumbers(schema: String): Set[NumberRecord] = {
    @tailrec
    def helper(s: List[Char], n: Int, length: Option[Int], accumulator: Seq[(Int, Int)]): Seq[(Int, Int)] =
      (s, length) match {
        case (Nil, None) =>
          accumulator
        case (Nil, Some(len)) =>
          (n - 1 - len, n - 1) +: accumulator
        case (head :: tail, None) if !head.isDigit =>
          helper(tail, n + 1, None, accumulator)
        case (head :: tail, None) if head.isDigit =>
          helper(tail, n + 1, Some(0), accumulator)
        case (head :: tail, Some(len)) if !head.isDigit =>
          helper(tail, n + 1, None, (n - 1 - len, n - 1) +: accumulator)
        case (head :: tail, Some(len)) if head.isDigit =>
          helper(tail, n + 1, Some(len + 1), accumulator)
      }

    schema
      .split("\n")
      .map(line => (line, helper(line.toList, 0, None, Seq.empty)))
      .zipWithIndex
      .flatMap { case ((line, locations), rowId) =>
        locations.map { case (from, to) =>
          NumberRecord(line.substring(from, to + 1).toInt, Region(Point(rowId, from), Point(rowId, to)))
        }
      }.toSet
  }
}
