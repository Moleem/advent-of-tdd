package day12.solvers

import utils.ProblemSolver

import scala.collection.mutable

object ManualPresolver extends ProblemSolver[List[(String, List[Int])], List[(String, List[Int])]] {
  override def solve(input: List[(String, List[Int])]): List[(String, List[Int])] =
    input.map { case (pattern, nums) =>
      val insideMargin = pattern.length - nums.sum - nums.size
      val sideMargin = insideMargin - 1



      (pattern, nums)
    }

}