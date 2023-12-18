package day18.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HoleInteriorDiggerSpec extends AnyFlatSpec with Matchers {

  behavior of HoleInteriorDigger.getClass.getSimpleName

  it should "correctly dig the hole's interior" in {
    val input =
      """#######
        |#.....#
        |###...#
        |..#...#
        |..#...#
        |###.###
        |#...#..
        |##..###
        |.#....#
        |.######""".stripMargin

    val expectedOutput =
      """#######
        |#######
        |#######
        |..#####
        |..#####
        |#######
        |#####..
        |#######
        |.######
        |.######""".stripMargin

    HoleInteriorDigger.solve(input) shouldBe expectedOutput
  }

}
