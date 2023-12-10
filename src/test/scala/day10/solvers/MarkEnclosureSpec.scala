package day10.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkEnclosureSpec extends AnyFlatSpec with Matchers {

  behavior of MarkEnclosure.getClass.getName

  it should "create an outer border around the original image" in {
    val input = List(
      List('.')
    )
    val expectedOutput = List(
      List('.', '.', '.'),
      List('.', '.', '.'),
      List('.', '.', '.')
    )

    MarkEnclosure.solve(input) shouldBe expectedOutput
  }

}
