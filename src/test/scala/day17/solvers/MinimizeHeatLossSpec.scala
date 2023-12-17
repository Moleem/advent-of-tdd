package day17.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class MinimizeHeatLossSpec extends AnyFlatSpec with Matchers {

  behavior of "MinimizeHeatLoss"

  it should "find the path with the least heat loss" in {
    val input = """2413432311323
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

    MinimizeHeatLoss.solve(input) shouldBe 102
  }

}
