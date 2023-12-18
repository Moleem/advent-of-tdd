package day18.solvers

import utils.ProblemSolver

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

  private def flood(tbf: Set[(Int, Int)], m: List[List[Char]]): List[List[Char]] = {
    val toBeFlooded = new mutable.Stack[(Int, Int)]()
    toBeFlooded.pushAll(tbf)

    val matrix = new ListBuffer[ListBuffer[Char]]()
    m.indices. foreach { row =>
      val thisRow = new ListBuffer[Char]()
      m.head.indices.foreach { col =>
        thisRow.addOne(m(row)(col))
      }
      matrix.addOne(thisRow)
    }


    while(toBeFlooded.nonEmpty) {
      val next = toBeFlooded.pop()

      if (matrix(next._1)(next._2) == '#') {
        // do nothing
      } else {
        val neighborsToFlood = Set(
          (next._1 + 1, next._2),
          (next._1 - 1, next._2),
          (next._1, next._2 + 1),
          (next._1, next._2 - 1)
        ).filter { case (row, col) => row >= 0 && col >= 0 && row < matrix.size && col < matrix.head.size }
          .filter { case (row, col) => matrix(row)(col) == 'x' }

        toBeFlooded.pushAll(neighborsToFlood)

        matrix(next._1)(next._2) = '.'
      }
    }



    matrix.map(_.toList).toList
  }
}
