package day12.solvers

import utils.ProblemSolver

import scala.collection.mutable

object ManualPresolver extends ProblemSolver[List[(String, List[Int])], List[(String, List[Int])]] {
  override def solve(input: List[(String, List[Int])]): List[(String, List[Int])] =
    input.map { case (pattern, nums) =>
      val uncertainty = pattern.length - (nums.sum + (nums.size-1))

      val sb = new StringBuilder
      (0 until uncertainty).foreach(_ => sb.append("_"))
      nums.foreach { num =>
        if (num > uncertainty)
          (0 until num-uncertainty).foreach(_ => sb.append("X"))
        else
          (0 until (uncertainty-num)).foreach(_ => sb.deleteCharAt(sb.length-1))

        (0 until uncertainty+1).foreach(_ => sb.append("_"))
      }

      sb.deleteCharAt(sb.length-1)

      if (sb.count(_=='X') == nums.sum) {
        sb.indices.foreach(id => if (sb.charAt(id) == '_') sb.replace(id, id+1, "O"))
      }

      (sb.toString, nums)
    }

}