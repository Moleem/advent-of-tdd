package day15.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.collection.immutable.Queue

case class Lens(label: String, focalLength: Int)

object Boxer extends ProblemSolver[String, Map[Int, Queue[Lens]]] {
  override def solve(input: String): Map[Int, Queue[Lens]] = {
    input.split(",").map { operation =>
      val parts = operation.split("=", -1)
      val label = parts(0)
      val focalLength = parts(1).toInt
      val index = Hasher.solve(label)
      (index, Queue(Lens(label, focalLength)))
    }.toMap
  }
}

class BoxerSpec extends AnyFlatSpec with Matchers {

  behavior of Boxer.getClass.getSimpleName

  it should "correctly insert a lens to a new bucket" in {
    val input = "rn=1"
    val expectedResult = Map(
      0 -> Queue(Lens("rn", 1))
    )
    Boxer.solve(input) shouldBe expectedResult
  }

  it should "correctly insert a second lens to a different bucket" in {
    val input = "rn=1,qp=3"
    val expectedResult = Map(
      0 -> Queue(Lens("rn", 1)),
      1 -> Queue(Lens("qp", 3))
    )
    Boxer.solve(input) shouldBe expectedResult
  }

  it should "correctly insert a second lens to a further bucket" in {
    val input = "pc=4"
    val expectedResult = Map(
      3 -> Queue(Lens("pc", 4))
    )
    Boxer.solve(input) shouldBe expectedResult
  }

}
