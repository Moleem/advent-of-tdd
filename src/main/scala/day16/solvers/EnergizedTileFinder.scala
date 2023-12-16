package day16.solvers

import day16.model.{Direction, DownLeaningMirror, EmptyTile, HorizontalSplitter, Tile, UpLeaningMirror, VerticalSplitter}
import utils.ProblemSolver

import scala.annotation.tailrec

class EnergizedTileFinder(startRow: Int, startCol: Int, direction: Direction) extends ProblemSolver[String, String] {

  override def solve(input: String): String = {
    val matrix = input.split("\n").map(_.toList).toList
    val rowCount = matrix.size
    val colCount = matrix.head.size

    val tiles = getTiles(matrix)

    val energizedTileCoordinates = getEnergizedTileCoordinates(
      lightSourcesToCheck = List(((startRow, startCol), direction)),
      tiles = tiles,
      lightSourcesChecked = List.empty,
      resultAccumulator = List.empty
    )

    drawEnergizedTileMap(rowCount, colCount, energizedTileCoordinates)
  }

  private def getTiles(matrix: List[List[Char]]): Map[(Int, Int), Tile] =
    matrix.indices.flatMap { row =>
      matrix.head.indices.map { col =>
        (row, col) -> (matrix(row)(col) match {
          case '.' => EmptyTile
          case '-' => HorizontalSplitter
          case '|' => VerticalSplitter
          case '\\' => DownLeaningMirror
          case '/' => UpLeaningMirror
        })
      }
    }.toMap

  @tailrec
  private def getEnergizedTileCoordinates(
                                           lightSourcesToCheck: List[((Int, Int), Direction)],
                                           tiles: Map[(Int, Int), Tile],
                                           lightSourcesChecked: List[((Int, Int), Direction)],
                                           resultAccumulator: List[(Int, Int)]): List[(Int, Int)] =
    lightSourcesToCheck match {
      case Nil => resultAccumulator
      case (coordinates, direction) :: tail =>
        val state = drawEnergizedTileMap(10, 10, resultAccumulator)
        tiles.get(coordinates) match {
          case Some(tile) if !lightSourcesChecked.contains(coordinates, direction) =>
            getEnergizedTileCoordinates(tile.handleLight(coordinates, direction) ++ tail, tiles, (coordinates, direction) :: lightSourcesChecked, coordinates :: resultAccumulator)
          case _ =>
            getEnergizedTileCoordinates(tail, tiles, (coordinates, direction) :: lightSourcesChecked, coordinates :: resultAccumulator)

        }
    }

  private def drawEnergizedTileMap(rowCount: Int, colCount: Int, energizedTileCoordinates: List[(Int, Int)]): String =
    (0 until rowCount).map { row =>
      (0 until colCount).map { col =>
        if (energizedTileCoordinates.contains((row, col))) '#' else '.'
      }.mkString
    }.mkString("\n")
}
