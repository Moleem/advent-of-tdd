package day12.solvers

import utils.ProblemSolver

import scala.annotation.tailrec

object SpringErrorMatcher extends ProblemSolver[List[(String, List[Int])], Int] {

  private def getAllPossibleValues(pattern: String) = {
    @tailrec
    def helper(toBeChecked: List[String], results: List[String]): List[String] = {
      toBeChecked match {
        case Nil => results
        case head :: tail if !head.contains("?") =>
          helper(tail, head :: results)
        case head :: tail if head.contains("?") =>
          val indexOfUnknown = head.indexOf("?")
          val withGood = head.substring(0, indexOfUnknown) + "." + head.substring(indexOfUnknown+1)
          val withBad = head.substring(0, indexOfUnknown) + "#" + head.substring(indexOfUnknown+1)
          helper(List(withGood, withBad) ++ tail, results)
      }
    }

    helper(List(pattern), List.empty)
  }

  private def getErrorClusters(s: String): List[Int] =
    s.split("\\.").toList.filterNot(_.isEmpty).map(_.length)

  /**
   *
   * .#.?.
   * .#..., .#.#.
   *
   *
   * */
  override def solve(input: List[(String, List[Int])]): Int = {
    input
      .map { case (pattern, nums) =>
        val possibleValues = getAllPossibleValues(pattern)
        val errorClusters = possibleValues.map(getErrorClusters)
        errorClusters.count(_ == nums)
      }.sum
  }
}