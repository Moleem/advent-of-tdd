package day14.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object LoadCalculator extends ProblemSolver[String, Int] {
  override def solve(input: String): Int = {
    input
      .split("\n")
      .toList
      .reverse
      .zipWithIndex
      .map { case (row, rowId) =>
        (rowId+1)*row.count(_=='O')
      }
      .sum
  }
}

class LoadCalculatorSpec extends AnyFlatSpec with Matchers {

  behavior of LoadCalculator.getClass.getSimpleName

  it should "calculate load correctly" in {
    val input = """OOOO.#.O..
                  |OO..#....#
                  |OO..O##..O
                  |O..#.OO...
                  |........#.
                  |..#....#.#
                  |..O..#.O.O
                  |..O.......
                  |#....###..
                  |#....#....""".stripMargin

    LoadCalculator.solve(input) shouldBe 136
  }
}
