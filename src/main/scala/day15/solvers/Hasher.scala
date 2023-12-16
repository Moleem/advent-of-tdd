package day15.solvers

import utils.ProblemSolver

import scala.annotation.tailrec

object Hasher extends ProblemSolver[String, Int] {
  override def solve(input: String): Int =
    input.split(",").map(_.toArray.toList).map(calculateHash).sum

  private def calculateHash(input: List[Char]): Int = {
    @tailrec
    def helper(input: List[Char], result: Int): Int =
      input match {
        case Nil => result
        case head :: tail => helper(tail, ((head.toInt + result) * 17) % 256)
      }

    helper(input, 0)
  }
}
