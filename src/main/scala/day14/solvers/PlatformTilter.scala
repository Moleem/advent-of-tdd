package day14.solvers

import utils.ProblemSolver

object PlatformTilter extends ProblemSolver[String, String] {
  override def solve(input: String): String = {
    val originalColumns = input.split("\n").map(_.toList).toList.transpose
    val tiltedColumns = originalColumns.map { column =>
      column.mkString.split("#", -1).map { partBetweenFixStones =>
        val (spaces, rollingStones) = partBetweenFixStones.partition(_ == 'O')
        spaces.mkString + rollingStones.mkString
      }.mkString("#")
    }

    tiltedColumns.transpose.map(_.mkString).mkString("\n")
  }
}
