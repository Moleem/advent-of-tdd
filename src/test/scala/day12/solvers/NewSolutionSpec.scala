package day12.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver


object NewSolution extends ProblemSolver[String, Long] {

  override def solve(input: String): Long = ???

}


class NewSolutionSpec extends AnyFlatSpec with Matchers {

  behavior of NewSolution.getClass.getSimpleName

  it should "count a single possible arrangement if there are no errors expected, and we dont know the values" in {
    val input = "??? "

    NewSolution.solve(input) shouldBe 1
  }

}
