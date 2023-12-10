package day10.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkEnclosureSpec extends AnyFlatSpec with Matchers {

  behavior of MarkEnclosure.getClass.getName

  it should "create an outer border around the original input" in {
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

  it should "keep the original pipes intact" in {
    val input = List(
      List('S','7'),
      List('L','J')
    )
    val expectedOutput = List(
      List('.', '.', '.', '.'),
      List('.', 'S', '7', '.'),
      List('.', 'L', 'J', '.'),
      List('.', '.', '.', '.')
    )

    MarkEnclosure.solve(input) shouldBe expectedOutput
  }

}
