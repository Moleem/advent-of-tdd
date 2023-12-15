package day15.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.annotation.tailrec

object Hasher extends ProblemSolver[String, Int] {
  override def solve(input: String): Int =
    input.split(",").map(_.toArray.toList).map(calculateHash).sum

  private def calculateHash(input: List[Char]): Int = {
    @tailrec
    def helper(input: List[Char], result: Int): Int =
      input match {
        case Nil => result
        case head::tail => helper(tail, ((head.toInt + result) * 17 ) % 256)
      }

    helper(input, 0)
  }
}

class HasherSpec extends AnyFlatSpec with Matchers {

  behavior of Hasher.getClass.getSimpleName

  it should "hash correctly" in {
    val input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    Hasher.solve(input) shouldBe 1320
  }

}
