package day15.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.collection.immutable.Queue

case class Lens(label: String, focalLength: Int)

object Boxer extends ProblemSolver[String, List[Queue[Lens]]] {
  override def solve(input: String): List[Queue[Lens]] = ???
}

class BoxerSpec extends AnyFlatSpec with Matchers {

  behavior of Boxer.getClass.getSimpleName

  it should "correctly insert a lens" in {
    val input = "rn=1"
    val expectedResult = List(Queue(Lens("rn", 1)))
    Boxer.solve(input) shouldBe expectedResult
  }

}
