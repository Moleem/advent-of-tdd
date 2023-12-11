package day11.parsers

import utils.ContentParser

import scala.collection.mutable

object GalaxyMapParser extends ContentParser[ Map[Int, (Int, Int)]] {
  override def parse(content: String):  Map[Int, (Int, Int)] = {
    val matrix = content.split("\n").toList.map(_.toList)

    matrix.indices.flatMap { row =>
      matrix.head.indices.map { col =>
        (row, col)
      }
    }
      .filter { case (row, col) => matrix(row)(col) == '#'}
      .zipWithIndex
      .map {case ((row, col), id) => id+1 -> (row, col) }
      .toMap
  }
}
