package day15.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

object Hasher extends ProblemSolver[String, Int] {
  override def solve(input: String): Int = ???
}

class HasherSpec extends AnyFlatSpec with Matchers {

  behavior of Hasher.getClass.getSimpleName

  it should "hash correctly" in {
    val input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    Hasher.solve(input) shouldBe 1320
  }

}
