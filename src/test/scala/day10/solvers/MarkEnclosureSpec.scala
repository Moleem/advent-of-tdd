package day10.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkEnclosureSpec extends AnyFlatSpec with Matchers {

  behavior of MarkEnclosure.getClass.getName

  it should "create an outer border around the original input" in {
    val input = List(
      List('S', '7'),
      List('L', 'J')
    )
    val expectedOutput = List(
      List(' ', ' ', ' ', ' '),
      List(' ', 'F', '7', ' '),
      List(' ', 'L', 'J', ' '),
      List(' ', ' ', ' ', ' ')
    )

    MarkEnclosure.solve(input) shouldBe expectedOutput
  }

  it should "fill with space anything that (even transitively) touches the wall" in {
    val input = List(
      List('S', '-', '-', '7'),
      List('|', 'F', '7', '|'),
      List('|', '|', '|', '|'),
      List('L', 'J', 'L', 'J')
    )
    val expectedOutput = List(
      List(' ', ' ', ' ', ' ', ' ', ' '),
      List(' ', 'F', '-', '-', '7', ' '),
      List(' ', '|', 'F', '7', '|', ' '),
      List(' ', '|', '|', '|', '|', ' '),
      List(' ', 'L', 'J', 'L', 'J', ' '),
      List(' ', ' ', ' ', ' ', ' ', ' ')
    )

    MarkEnclosure.solve(input) shouldBe expectedOutput
  }

  it should "fill with space anything that (even transitively) touches the wall, even within the shape" in {
    val input = List(
      List('S', '-', '-', '-', '7'),
      List('|', 'F', '-', '7', '|'),
      List('|', '|', ' ', '|', '|'),
      List('L', 'J', ' ', 'L', 'J')
    )
    val expectedOutput = List(
      List(' ', ' ', ' ', ' ', ' ', ' ', ' '),
      List(' ', 'F', '-', '-', '-', '7', ' '),
      List(' ', '|', 'F', '-', '7', '|', ' '),
      List(' ', '|', '|', ' ', '|', '|', ' '),
      List(' ', 'L', 'J', ' ', 'L', 'J', ' '),
      List(' ', ' ', ' ', ' ', ' ', ' ', ' ')
    )

    MarkEnclosure.solve(input) shouldBe expectedOutput
  }


  it should "replace any non-main-pipe with space" in {
    val input = List(
      List('S', '-', '-', '-', '7'),
      List('|', 'F', '-', '7', '|'),
      List('|', '|', '.', '|', '|'),
      List('L', 'J', '#', 'L', 'J')
    )
    val expectedOutput = List(
      List(' ', ' ', ' ', ' ', ' ', ' ', ' '),
      List(' ', 'F', '-', '-', '-', '7', ' '),
      List(' ', '|', 'F', '-', '7', '|', ' '),
      List(' ', '|', '|', ' ', '|', '|', ' '),
      List(' ', 'L', 'J', ' ', 'L', 'J', ' '),
      List(' ', ' ', ' ', ' ', ' ', ' ', ' ')
    )

    MarkEnclosure.solve(input) shouldBe expectedOutput
  }

  it should "fill enclosed space with X" in {
    val input = List(
      List('S', '-', '7'),
      List('|', 'X', '|'),
      List('L', '-', 'J')
    )
    val expectedOutput = List(
      List(' ', ' ', ' ', ' ', ' '),
      List(' ', 'F', '-', '7', ' '),
      List(' ', '|', 'X', '|', ' '),
      List(' ', 'L', '-', 'J', ' '),
      List(' ', ' ', ' ', ' ', ' ')
    )

    MarkEnclosure.solve(input) shouldBe expectedOutput
  }


}
