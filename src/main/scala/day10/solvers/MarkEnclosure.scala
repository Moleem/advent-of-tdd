package day10.solvers

import utils.ProblemSolver

import scala.annotation.tailrec

object MarkEnclosure extends ProblemSolver[List[List[Char]], List[List[Char]]] {
  override def solve(input: List[List[Char]]): List[List[Char]] = {
    val rowCount = input.size + 2
    val colCount = input.head.size + 2

    (0 until rowCount).map { row =>
      (0 until colCount).map { col =>
        if (row == 0 || row == rowCount-1 || col == 0 || col == colCount -1) '.'
        else input(row-1)(col-1)
      }.toList
    }.toList


  }

}
