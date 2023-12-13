package day12.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.util.Try


object NewSolution extends ProblemSolver[String, Long] {

  override def solve(input: String): Long = {
    val Array(pattern, groupsStr) = input.split(" ", 2).map(_.toString)
    val groups = Try(groupsStr.split(",").map(_.toInt).toList).getOrElse(List.empty[Int])

    countArrangements(pattern, groups)
  }

  private def countArrangements(pattern: String, groups: List[Int], resultAccumulator: Long = 0): Long =
    (pattern, groups) match {
      case (p, Nil) if !p.contains('#') => 1
    }

}


class NewSolutionSpec extends AnyFlatSpec with Matchers {

  behavior of NewSolution.getClass.getSimpleName

  it should "count a single possible arrangement if there are no errors expected, and nor present" in {
    NewSolution.solve(" ") shouldBe 1
    NewSolution.solve("? ") shouldBe 1
    NewSolution.solve(". ") shouldBe 1
    NewSolution.solve(".? ") shouldBe 1
  }

}
