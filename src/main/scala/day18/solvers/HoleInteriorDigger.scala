package day18.solvers

import utils.ProblemSolver

import scala.annotation.tailrec

object HoleInteriorDigger extends ProblemSolver[String, String] {
  override def solve(input: String): String = {
    val matrix = input.split("\n").map(_.toList).toList

    val inputWithBorder = withBorder(matrix)
    val prefilled = replaceAll(inputWithBorder, '.', 'x')
    val flooded = flood(Set((0, 0)), prefilled)
    val filled = replaceAll(flooded, 'x', '#')

    filled.map(_.mkString).mkString("\n")
  }

  private def withBorder(m: List[List[Char]]): List[List[Char]] = {
    val originalRowCount = m.size
    val originalColCount = m.head.size

    val topBottomRow = List.tabulate(originalColCount+2)(_ => '.')
    val middleRows = m.map(row => '.' +: row :+ '.')

    topBottomRow +: middleRows :+ topBottomRow
  }

  private def replaceAll(m: List[List[Char]], from: Char, to: Char): List[List[Char]] =
    m.indices.map { row =>
      m.head.indices.map { col =>
        if (m(row)(col) == from) to
        else m(row)(col)
      } .toList
    }.toList

  @tailrec
  private def flood(toBeFlooded: Set[(Int, Int)], m: List[List[Char]]): List[List[Char]] =
    if (toBeFlooded.isEmpty) {
      m
    } else {
      val next = toBeFlooded.head
      if (m(next._1)(next._2) == '#')
        flood(toBeFlooded.tail, m)
      else {
        val neighborsToFlood = Set(
          (next._1 + 1, next._2),
          (next._1 - 1, next._2),
          (next._1, next._2 + 1),
          (next._1, next._2 - 1)
        ).filter { case (row, col) => row >= 0 && col >= 0 && row < m.size && col < m.head.size }
          .filter { case (row, col) => m(row)(col) == 'x' }

        val newM = m.indices.map { row =>
          m.head.indices.map { col =>
            if (row == next._1 && col == next._2) '.'
            else m(row)(col)
          }.toList
        }.toList

        flood(neighborsToFlood ++ toBeFlooded.tail, newM)
      }
    }
}
