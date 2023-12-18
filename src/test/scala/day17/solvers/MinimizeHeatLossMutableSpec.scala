package day17.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class MinimizeHeatLossMutableSpec extends AnyFlatSpec with Matchers {

  behavior of "MinimizeHeatLossMutable"

  it should "find the path with the least heat loss" in {
    val input =
      """2413432311323
        |3215453535623
        |3255245654254
        |3446585845452
        |4546657867536
        |1438598798454
        |4457876987766
        |3637877979653
        |4654967986887
        |4564679986453
        |1224686865563
        |2546548887735
        |4322674655533""".stripMargin

    MinimizeHeatLossMutable.solve(input) shouldBe 102
  }

  it should "find the path with the least heat loss in a small example (1)" in {
    val input =
      """11111
        |11111
        |11111
        |11111
        |11111""".stripMargin

    MinimizeHeatLossMutable.solve(input) shouldBe 8
  }

  it should "find the path with the least heat loss in a small example (2)" in {
    val input =
      """11111
        |11111
        |11111
        |11111
        |11112""".stripMargin

    MinimizeHeatLossMutable.solve(input) shouldBe 9
  }

  it should "find the path with the least heat loss in a small example (3)" in {
    val input =
      """21111
        |11111
        |11111
        |11111
        |11111""".stripMargin

    MinimizeHeatLossMutable.solve(input) shouldBe 8
  }

  it should "find the path with the least heat loss in a small example (4)" in {
    val input =
      """21111
        |11121
        |11111
        |12111
        |11111""".stripMargin

    MinimizeHeatLossMutable.solve(input) shouldBe 8
  }
  it should "find the path with the least heat loss in a small example (5)" in {
    val input =
      """12222
        |11111""".stripMargin

    MinimizeHeatLossMutable.solve(input) shouldBe 6
  }

}
