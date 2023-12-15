package day15.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

case class Lens(label: String, focalLength: Int)

object Boxer extends ProblemSolver[String, Map[Int, List[Lens]]] {
  override def solve(input: String): Map[Int, List[Lens]] = {
    input.split(",").foldLeft(Map.empty[Int, List[Lens]]) { case (boxes, operation) =>
      if (operation.contains('=')) {
        val parts = operation.split("=", -1)
        val label = parts(0)
        val focalLength = parts(1).toInt
        val index = Hasher.solve(label)

        boxes.updatedWith(index) {
          case None => Some(List(Lens(label, focalLength)))
          case Some(existingBoxContent) =>
            if (existingBoxContent.exists(_.label == label))
              Some(existingBoxContent.map(lens => if (lens.label == label) lens.copy(focalLength = focalLength) else lens))
            else
              Some(existingBoxContent :+ Lens(label, focalLength))
        }
      } else {
        val label = operation.stripSuffix("-")
        val index = Hasher.solve(label)

        boxes.updatedWith(index) {
            case None => None
            case Some(existingBoxContent) => Some(existingBoxContent.filterNot(_.label == label))
        }
      }
    }.filterNot(_._2.isEmpty)
  }
}

class BoxerSpec extends AnyFlatSpec with Matchers {

  behavior of Boxer.getClass.getSimpleName

  it should "correctly insert a lens to a new bucket" in {
    val input = "rn=1"
    val expectedResult = Map(
      0 -> List(Lens("rn", 1))
    )
    Boxer.solve(input) shouldBe expectedResult
  }

  it should "correctly insert a second lens to a different bucket" in {
    val input = "rn=1,qp=3"
    val expectedResult = Map(
      0 -> List(Lens("rn", 1)),
      1 -> List(Lens("qp", 3))
    )
    Boxer.solve(input) shouldBe expectedResult
  }

  it should "correctly insert a second lens to a further bucket" in {
    val input = "pc=4"
    val expectedResult = Map(
      3 -> List(Lens("pc", 4))
    )
    Boxer.solve(input) shouldBe expectedResult
  }

  it should "correctly insert a lens into an existing bucket" in {
    val input = "rn=1,cm=2"
    val expectedResult = Map(
      0 -> List(Lens("rn", 1), Lens("cm", 2))
    )
    Boxer.solve(input) shouldBe expectedResult
  }

  it should "correctly remove a lens" in {
    val input = "rn=1,rn-"
    val expectedResult = Map.empty
    Boxer.solve(input) shouldBe expectedResult
  }

  it should "correctly replace a lens" in {
    val input = "rn=1,rn=2"
    val expectedResult = Map(
      0 -> List(Lens("rn", 2))
    )
    Boxer.solve(input) shouldBe expectedResult
  }

  it should "work on the original example" in {
    val input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    val expectedResult = Map(
      0 -> List(Lens("rn", 1), Lens("cm", 2)),
      3 -> List(Lens("ot", 7), Lens("ab", 5), Lens("pc", 6))
    )
    Boxer.solve(input) shouldBe expectedResult
  }

}
