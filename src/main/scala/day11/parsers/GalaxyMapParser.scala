package day11.parsers

import utils.ContentParser

object GalaxyMapParser extends ContentParser[ Map[Int, (Int, Int)]] {
  override def parse(content: String):  Map[Int, (Int, Int)] = {
    val matrix = content.split("\n").toList.map(_.toList)

    def getNumberOfEmptyRowsBefore(row: Int): Int =
      (0 until row).count(id => matrix(id).forall(_ == '.'))


    val galaxies = matrix.indices.flatMap { row =>
      matrix.head.indices.map { col =>
        (row, col)
      }
    }
      .filter { case (row, col) => matrix(row)(col) == '#'}
      .zipWithIndex
      .map {case ((row, col), id) => id -> (row, col) }
      .toMap

    galaxies.map { case (id, (row, col)) =>
      val numberOfEmptyRowsBefore = getNumberOfEmptyRowsBefore(row)
      id -> (row + numberOfEmptyRowsBefore, col)
    }
  }

}
