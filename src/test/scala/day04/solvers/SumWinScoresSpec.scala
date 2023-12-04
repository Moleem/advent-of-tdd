package day04.solvers

import day04.model.ScratchCardRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object SumWinScores extends ProblemSolver[List[ScratchCardRecord], Int] {
  override def solve(input: List[ScratchCardRecord]): Int = ???
}

class SumWinScoresSpec extends AnyFlatSpec with Matchers {

  behavior of "SumWinScores"

  it should "correctly score 0 matches" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(4, 5, 6))
    )

    SumWinScores.solve(input) shouldBe 0
  }

  it should "correctly score 1 match" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(3, 4, 5))
    )

    SumWinScores.solve(input) shouldBe 1
  }

  it should "correctly score 2 matches" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(2, 3, 4))
    )

    SumWinScores.solve(input) shouldBe 2
  }

  it should "correctly score 3 matches" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(1, 2, 3))
    )

    SumWinScores.solve(input) shouldBe 4
  }

  it should "correctly score 4 matches" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3, 4), List(1, 2, 3, 4))
    )

    SumWinScores.solve(input) shouldBe 8
  }

  it should "correctly sum match scores across records" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(1, 2, 3)), // 3 matches, 4 points
      ScratchCardRecord(List(1, 2, 3), List(2, 3, 4)), // 2 matches, 2 points
      ScratchCardRecord(List(1, 2, 3), List(3, 4, 5)), // 1 matche,  1 points
      ScratchCardRecord(List(1, 2, 3), List(4, 5, 6)), // 0 matches, 0 points
    )

    SumWinScores.solve(input) shouldBe 7 // 4 + 2 + 1 + 0
  }

}
