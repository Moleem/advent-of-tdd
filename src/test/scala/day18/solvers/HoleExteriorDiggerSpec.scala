package day18.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HoleExteriorDiggerSpec extends AnyFlatSpec with Matchers {

  behavior of HoleExteriorDigger.getClass.getSimpleName

  it should "correctly dig the hole's exterior" in {
    val input = """R 6 (#70c710)
                  |D 5 (#0dc571)
                  |L 2 (#5713f0)
                  |D 2 (#d2c081)
                  |R 2 (#59c680)
                  |D 2 (#411b91)
                  |L 5 (#8ceee2)
                  |U 2 (#caa173)
                  |L 1 (#1b58a2)
                  |U 2 (#caa171)
                  |R 2 (#7807d2)
                  |U 3 (#a77fa3)
                  |L 2 (#015232)
                  |U 2 (#7a21e3)""".stripMargin

    val expectedOutput = """#######
                           |#.....#
                           |###...#
                           |..#...#
                           |..#...#
                           |###.###
                           |#...#..
                           |##..###
                           |.#....#
                           |.######""".stripMargin

    HoleExteriorDigger.solve(input) shouldBe expectedOutput
  }
}
