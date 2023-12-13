package day13.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MirrorFinderSpec extends AnyFlatSpec with Matchers {

  behavior of MirrorFinder.getClass.getSimpleName

  it should "identify a vertical mirror" in {
    val input =
      """#.##..##.
        |..#.##.#.
        |##......#
        |##......#
        |..#.##.#.
        |..##..##.
        |#.#.##.#.""".stripMargin

    MirrorFinder.parse(input) shouldBe 5
  }

  it should "identify a horizontal mirror (with a 100 multiplier)" in {
    val input =
      """#...##..#
        |#....#..#
        |..##..###
        |#####.##.
        |#####.##.
        |..##..###
        |#....#..#""".stripMargin

    MirrorFinder.parse(input) shouldBe 400
  }

  it should "sum results" in {
    val input =
      """#.##..##.
        |..#.##.#.
        |##......#
        |##......#
        |..#.##.#.
        |..##..##.
        |#.#.##.#.
        |
        |#...##..#
        |#....#..#
        |..##..###
        |#####.##.
        |#####.##.
        |..##..###
        |#....#..#""".stripMargin

    MirrorFinder.parse(input) shouldBe 405
  }

}
