package day15.solvers

import day15.model.Lens
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object BoxSummarizer extends ProblemSolver[Map[Int, List[Lens]], Int] {

  override def solve(input: Map[Int, List[Lens]]): Int =
    input.flatMap { case (boxId, lenses) =>
      lenses.zipWithIndex.map { case (lens, lensId) =>
        (boxId+1) * (lensId+1) * lens.focalLength
      }
    }.sum

}

class BoxSumamrizerSpec extends AnyFlatSpec with Matchers {

  behavior of BoxSummarizer.getClass.getSimpleName

  it should "summarize box contents" in {
    val input = Map(
      0 -> List(Lens("rn", 1), Lens("cm", 2)),
      3 -> List(Lens("ot", 7), Lens("ab", 5), Lens("pc", 6))
    )
    val expectedResult = 145

    BoxSummarizer.solve(input) shouldBe expectedResult
  }

}
