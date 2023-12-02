package day02.solvers

import day02.model.GameRecord
import utils.ProblemSolver

object SumPowerMins extends ProblemSolver[Seq[GameRecord], Int] {

  private def getMins(draws: Seq[Map[String, Int]]): Map[String, Int] =
    draws.reduce[Map[String, Int]] { case (drawA: Map[String, Int], drawB: Map[String, Int]) =>
      val allKeys = drawA.keys ++ drawB.keys

      allKeys.map { key =>
        key -> Seq(drawA.getOrElse(key, 0), drawB.getOrElse(key, 0)).max
      }.toMap
    }

  private def getPower(mins: Map[String, Int]): Int =
    mins.values.product

  override def solve(input: Seq[GameRecord]): Int =
    input
      .map(_.rounds)
      .map(getMins)
      .map(getPower)
      .sum

}
