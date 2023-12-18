package day18.solvers

import utils.ProblemSolver

object HoleExteriorDigger extends ProblemSolver[String, String] {
  override def solve(input: String): String = {
    val lines = input.split("\n").toList

    val instructions: List[(Char, Int)] = lines.map { line =>
      val parts = line.split(" ")
      (parts(0).head, parts(1).toInt)
    }

    var row = 0;
    var col = 0;
    val holeExterior = new scala.collection.mutable.HashSet[(Int, Int)]()
    holeExterior.addOne((row, col))

    instructions.foreach { case (direction, distance) =>
      (0 until distance).foreach { _ =>
        direction match {
          case 'R' => col += 1
          case 'L' => col -= 1
          case 'U' => row -= 1
          case 'D' => row += 1
        }
        holeExterior.addOne((row, col))
      }
    }

    val minRow = holeExterior.map(_._1).min
    val maxRow = holeExterior.map(_._1).max
    val minCol = holeExterior.map(_._2).min
    val maxCol = holeExterior.map(_._2).max

    (minRow to maxRow).map { row =>
      (minCol to maxCol).map { col =>
        if (holeExterior.contains((row, col)))
          '#'
        else
          '.'
      }.mkString
    }.mkString("\n")
  }
}
