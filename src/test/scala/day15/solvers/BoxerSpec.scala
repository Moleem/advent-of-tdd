package day15.solvers

import day15.model.Lens
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


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
