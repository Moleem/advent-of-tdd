package day13.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SmudgeMirrorFinderSpec extends AnyFlatSpec with Matchers {

  behavior of SmudgeMirrorFinder.getClass.getSimpleName

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

    SmudgeMirrorFinder.parse(input) shouldBe 400
  }

}
