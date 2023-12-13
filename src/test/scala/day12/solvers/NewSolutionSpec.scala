package day12.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.util.Try


object NewSolution extends ProblemSolver[String, Long] {

  override def solve(input: String): Long = {
    val Array(pattern, groupsStr) = input.split(" ", 2)
    val groups = Try(groupsStr.split(",").map(_.toInt).toList).getOrElse(List.empty[Int])

    countArrangements(pattern, groups)
  }

  private def countArrangements(pattern: String, groups: List[Int], resultAccumulator: Long = 0): Long =
    (pattern, groups) match {
      case (p, Nil) if !p.contains('#') => 1
      case (p, g::Nil) if p.length == g && !p.contains('.') => 1
    }

}


class NewSolutionSpec extends AnyFlatSpec with Matchers {

  behavior of NewSolution.getClass.getSimpleName

  it should "get arrangement count 1 if there are no errors expected, and nor present" in {
    NewSolution.solve(" ") shouldBe 1
    NewSolution.solve("? ") shouldBe 1
    NewSolution.solve(". ") shouldBe 1
    NewSolution.solve(".? ") shouldBe 1
  }

  it should "get arrangement count 1 if there are errors expected exactly fit the available space" in {
    NewSolution.solve("? 1") shouldBe 1
    NewSolution.solve("# 1") shouldBe 1
    NewSolution.solve("?? 2") shouldBe 1
    NewSolution.solve("?# 2") shouldBe 1
    NewSolution.solve("#? 2") shouldBe 1
    NewSolution.solve("## 2") shouldBe 1
  }

  it should "get arrangement count 0 if there are errors expected that do not fit the available space" in {
    NewSolution.solve("? 2") shouldBe 0
    NewSolution.solve("# 2") shouldBe 0
    NewSolution.solve("?. 2") shouldBe 0
    NewSolution.solve(".? 2") shouldBe 0
    NewSolution.solve("#. 2") shouldBe 0
    NewSolution.solve(".# 2") shouldBe 0
  }

}
