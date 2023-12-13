package day12.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.util.Try


object NewSolution extends ProblemSolver[String, Long] {

  override def solve(input: String): Long = {
    input.split("\n").map{ line =>
      val Array(pattern, groupsStr) = line.split(" ", 2)
      val groups = Try(groupsStr.split(",").map(_.toInt).toList).getOrElse(List.empty[Int])

      val x = countArrangements(pattern, groups)
      println(s"$line -> $pattern, ${groups.mkString("(", ",", ")")} -> $x")
      x
    }.sum
  }

  private def countArrangements(pattern: String, groups: List[Int]): Long =
    (pattern, groups) match {
      case (p, Nil) if !p.contains('#') => 1
      case (p, g::Nil) if p.length == g && !p.contains('.') => 1
      case (p, g::Nil) if p.length < g => 0
      case (p, g::Nil) if p.contains('.') && p.split("\\.").forall(_.length < g) => 0
      case (p, g :: Nil) if p.length > g =>
        countArrangements(p.take(g), groups) +
        countArrangements(p.tail, groups)
      case (p, gHead::gTail) if p.length < gTail.sum + gTail.size + gHead => 0
      case (p, gHead::gTail) =>
        (0 until (p.length - gTail.sum - gTail.size - gHead + 1)).map { startIndex: Int =>
          if (!p.substring(startIndex).take(gHead).contains('.') && p.charAt(startIndex+gHead) != '#')
            countArrangements(p.substring(startIndex+gHead+1), gTail)
          else 0
        }.sum
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

  it should "get arrangement count 2 if there are errors expected that fit the available space in two ways" in {
    NewSolution.solve("??? 2") shouldBe 2
    NewSolution.solve("?#? 2") shouldBe 2
    NewSolution.solve("?#..?? 2") shouldBe 2
    NewSolution.solve("?? 1") shouldBe 2
    NewSolution.solve("?.? 1") shouldBe 2
  }

  it should "get arrangement count 0 if there are multiple errors expected, but they cannot fit" in {
    NewSolution.solve("??? 1,2") shouldBe 0
    NewSolution.solve("???? 1,1,1") shouldBe 0
  }

  it should "get arrangement count 1 even if there are multiple errors expected" in {
    NewSolution.solve("???? 1,2") shouldBe 1
    NewSolution.solve("?.?? 1,2") shouldBe 1
    NewSolution.solve("?.#? 1,2") shouldBe 1
    NewSolution.solve("#.#? 1,2") shouldBe 1
    NewSolution.solve("#.?# 1,2") shouldBe 1
    NewSolution.solve("#.## 1,2") shouldBe 1
  }

  it should "sum arrangement counts" in {
    val input =
      """? 1
        |?? 1
        |??? 1""".stripMargin

    NewSolution.solve(input) shouldBe 6
  }

  it should "solve the example task" in {
    val input = """???.### 1,1,3
                  |.??..??...?##. 1,1,3
                  |?#?#?#?#?#?#?#? 1,3,1,6
                  |????.#...#... 4,1,1
                  |????.######..#####. 1,6,5
                  |?###???????? 3,2,1""".stripMargin

    NewSolution.solve(input) shouldBe 21
  }

  it should "work in example case (3)" in {
    NewSolution.solve("?#?#?#?#?#?#?#? 1,3,1,6") shouldBe 1
  }

  it should "work in example case (6)" in {
    NewSolution.solve("?###???????? 3,2,1") shouldBe 10
  }
}
