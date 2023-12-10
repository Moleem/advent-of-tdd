package utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProblemSolverSpec extends AnyFlatSpec with Matchers {

  behavior of "ProblemSolver"

  it should "be chainable" in {
    val minFinder = new ProblemSolver[List[Int], Int] {
      override def solve(input: List[Int]): Int = input.min
    }

    val aRepeater = new ProblemSolver[Int, String] {
      override def solve(input: Int): String = (0 until input).map(_ => "A").mkString
    }

    minFinder.andThen(aRepeater).solve(List(3,4,5)) shouldBe "AAA"
  }

}
