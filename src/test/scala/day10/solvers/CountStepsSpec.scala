package day10.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountStepsSpec extends AnyFlatSpec with Matchers {

  behavior of CountSteps.getClass.getName

  it should "be able to count steps south" in {
    val input = List(
      List('S'),
      List('|'),
      List('S')
    )

    CountSteps.solve(input) shouldBe 2
  }

  it should "be able to count steps east" in {
    val input = List(
      List('S', '-', 'S')
    )

    CountSteps.solve(input) shouldBe 2
  }

  it should "be able to count steps south east" in {
    val input = List(
      List('S', '.'),
      List('L', 'S')
    )

    CountSteps.solve(input) shouldBe 2
  }

  it should "be able to count steps south west" in {
    val input = List(
      List('.', 'S'),
      List('S', 'J')
    )

    CountSteps.solve(input) shouldBe 2
  }

  it should "be able to count steps north east" in {
    val input = List(
      List('S', '.'),
      List('|', 'S'),
      List('L', 'J')
    )

    CountSteps.solve(input) shouldBe 4
  }

  it should "be able to count steps north" in {
    val input = List(
      List('S', 'S'),
      List('|', '|'),
      List('L', 'J')
    )

    CountSteps.solve(input) shouldBe 5
  }

  it should "be able to count steps north west" in {
    val input = List(
      List('S', '7'),
      List('|', '|'),
      List('L', 'J')
    )

    CountSteps.solve(input) shouldBe 6
  }

  it should "be able to count steps west" in {
    val input = List(
      List('S', '-', '7'),
      List('|', '.', '|'),
      List('L', '-', 'J')
    )

    CountSteps.solve(input) shouldBe 8
  }

  it should "be able to count steps west north" in {
    val input = List(
      List('S', '7', '.'),
      List('|', 'L', '7'),
      List('L', '-', 'J')
    )

    CountSteps.solve(input) shouldBe 8
  }

}
