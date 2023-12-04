package day04.solvers

import day04.model.ScratchCardRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver


object CountCards extends ProblemSolver[List[ScratchCardRecord], Int] {
  override def solve(input: List[ScratchCardRecord]): Int =
    ???
}


class CountCardsSpec extends AnyFlatSpec with Matchers {

  behavior of "CountCards"

  it should "correctly count a single, non-winning card" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(4, 5, 6)) // 0, counted 1 time(s)
    )

    SumWinScores.solve(input) shouldBe 1
  }

  it should "correctly count 1 winning card and it's non-winning reward card" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(3, 4, 5)), // 1, counted 1 time(s)
      ScratchCardRecord(List(1, 2, 3), List(4, 5, 6))  // 0, counted 2 time(s)
    )

    SumWinScores.solve(input) shouldBe 3
  }

  it should "correctly score 2 winning cards and their transitive non-winning reward cards" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(3, 4, 5)), // 1, counted 1 time(s)
      ScratchCardRecord(List(1, 2, 3), List(4, 5, 6)), // 0, counted 2 time(s)
      ScratchCardRecord(List(1, 2, 3), List(3, 4, 5)), // 1, counted 1 time(s)
      ScratchCardRecord(List(1, 2, 3), List(4, 5, 6))  // 0, counted 2 time(s)
    )

    SumWinScores.solve(input) shouldBe 6
  }

  it should "correctly count 1 winning card and it's winning reward card" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(3, 4, 5)), // 1, counted 1 time(s)
      ScratchCardRecord(List(1, 2, 3), List(3, 6, 8)), // 1, counted 2 time(s)
      ScratchCardRecord(List(1, 2, 3), List(4, 5, 6))  // 0, counted 3 time(s)
    )

    SumWinScores.solve(input) shouldBe 6
  }

  it should "correctly count multi-winning card and it's winning reward cards" in {
    val input = List(
      ScratchCardRecord(List(1, 2, 3), List(2, 3, 4)), // 2, counted 1 time(s)
      ScratchCardRecord(List(1, 2, 3), List(3, 6, 8)), // 1, counted 2 time(s)
      ScratchCardRecord(List(1, 2, 3), List(4, 5, 6))  // 0, counted 4 time(s)
    )

    SumWinScores.solve(input) shouldBe 7
  }

  it should "correctly count original example" in {
    val input = List(
      ScratchCardRecord(List(41, 48, 83, 86, 17), List(83, 86, 6, 31, 17, 9, 48, 53)),    // 4, counted 1  time(s)
      ScratchCardRecord(List(13, 32, 20, 16, 61), List(61, 30, 68, 82, 17, 32, 24, 19)),  // 2, counted 2  time(s)
      ScratchCardRecord(List(1, 21, 53, 59, 44),  List(69, 82, 63, 72, 16, 21, 14, 1)),   // 2, counted 4  time(s)
      ScratchCardRecord(List(41, 92, 73, 84, 69), List(59, 84, 76, 51, 58, 5, 54, 83)),   // 1, counted 8  time(s)
      ScratchCardRecord(List(87, 83, 26, 28, 32), List(88, 30, 70, 12, 93, 22, 82, 36)),  // 0, counted 14 time(s)
      ScratchCardRecord(List(31, 18, 13, 56, 72), List(74, 77, 10, 23, 35, 67, 36, 11)),  // 0, counted 1  time(s)
    )

    SumWinScores.solve(input) shouldBe 30
  }

}
