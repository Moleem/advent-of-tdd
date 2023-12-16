package day16.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.annotation.tailrec

sealed trait Tile {
  def handleLight(coordinates: (Int, Int), fromDirection: Direction): List[((Int, Int), Direction)]
}

case object EmptyTile extends Tile {
  override def handleLight(coordinates: (Int, Int), fromDirection: Direction): List[((Int, Int), Direction)] =
    fromDirection match {
      case Right => List(
        ((coordinates._1, coordinates._2+1), Right)
      )
    }
}

sealed trait Direction
case object Right extends Direction

class EnergizedTileFinder(startRow: Int, startCol: Int, direction: Direction) extends ProblemSolver[String, String] {

  override def solve(input: String): String = {
    val matrix = input.split("\n").map(_.toList).toList
    val rowCount = matrix.size
    val colCount = matrix.head.size

    val tiles = getTiles(matrix)

    val energizedTileCoordinates = getEnergizedTileCoordinates(
      lightSourcesToCheck = List(((startRow, startCol), direction)),
      tiles = tiles,
      resultAccumulator = List.empty[(Int, Int)]
    )

    drawEnergizedTileMap(rowCount, colCount, energizedTileCoordinates)
  }

  private def getTiles(matrix: List[List[Char]]): Map[(Int, Int), Tile] =
    matrix.indices.flatMap { row =>
      matrix.head.indices.map { col =>
        (row, col) -> (matrix(row)(col) match {
          case '.' => EmptyTile
        })
      }
    }.toMap

  @tailrec
  private def getEnergizedTileCoordinates(
                lightSourcesToCheck: List[((Int, Int), Direction)],
                tiles: Map[(Int, Int), Tile],
                resultAccumulator: List[(Int, Int)]): List[(Int, Int)] =
    lightSourcesToCheck match {
      case Nil => resultAccumulator
      case (coordinates, direction) :: tail =>
        tiles.get(coordinates) match {
          case None =>
            getEnergizedTileCoordinates(tail, tiles, coordinates :: resultAccumulator)
          case Some(tile) =>
            getEnergizedTileCoordinates(tail ++ tile.handleLight(coordinates, direction), tiles, coordinates :: resultAccumulator)
      }
    }

  private def drawEnergizedTileMap(rowCount: Int, colCount: Int, energizedTileCoordinates: List[(Int, Int)]): String =
    (0 until rowCount).map { row =>
      (0 until colCount).map { col =>
        if (energizedTileCoordinates.contains((row, col))) '#' else '.'
      }.mkString
    }.mkString("\n")
}

class EnergizedTileFinderSpec extends AnyFlatSpec with Matchers {

  behavior of "EnergizedTileFinder"

  it should "work (empty tile, beam travels right)" in {
    val input =
      """.....
        |.....
        |.....""".stripMargin
    val expectedOutput =
      """.....
        |#####
        |.....""".stripMargin

    new EnergizedTileFinder(1,0,Right).solve(input) shouldBe expectedOutput
  }

}
