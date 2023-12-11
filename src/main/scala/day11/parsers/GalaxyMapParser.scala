package day11.parsers

import utils.ContentParser

import scala.collection.mutable

object GalaxyMapParser extends ContentParser[ Map[Int, (Int, Int)]] {
  override def parse(content: String):  Map[Int, (Int, Int)] = {
    val matrix = content.split("\n").toList.map(_.toList)

    var galaxyCounter = 0
    var galaxies = new mutable.HashMap[Int, (Int, Int)]

    matrix.indices.map { row =>
      matrix.head.indices.map { col =>
        if (matrix(row)(col) == '#') {
          galaxyCounter += 1
          galaxies.put(galaxyCounter, (row, col))
        }
      }
    }

    galaxies.toMap
  }
}
