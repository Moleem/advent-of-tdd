package day15.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.collection.immutable.Queue

case class Lens(label: String, focalLength: Int)

object Boxer extends ProblemSolver[String, List[Queue[Lens]]] {
  override def solve(input: String): List[Queue[Lens]] = {
    val inputParts = input.split("=", -1)
    val label = inputParts(0)
    val focalLength = inputParts(1).toInt

    List(Queue(Lens(label, focalLength)))
  }
}

class BoxerSpec extends AnyFlatSpec with Matchers {

  behavior of Boxer.getClass.getSimpleName

  it should "correctly insert a lens" in {
    val input = "rn=1"
    val expectedResult = List(Queue(Lens("rn", 1)))
    Boxer.solve(input) shouldBe expectedResult
  }

}
