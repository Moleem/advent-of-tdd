package day03.misc

import day03.model.Point

object SymbolLocator {
  def locateSymbols(input: String): Set[Point] = {
    val matrix = input.split("\n").map(_.toList).toList

    matrix.indices.flatMap { row =>
      matrix.head.indices.map { col =>
        (row, col)
      }
    }
      .filter { case (row, col) => matrix(row)(col) != '.' && !matrix(row)(col).isDigit }
      .map { case (row, col) => Point(row, col) }
      .toSet
  }
}