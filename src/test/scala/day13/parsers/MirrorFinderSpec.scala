package day13.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MirrorFinderSpec extends AnyFlatSpec with Matchers {

  behavior of MirrorFinder.getClass.getSimpleName

  it should "identify a vertical mirror" in {
    val input = """#.##..##.
                  |..#.##.#.
                  |##......#
                  |##......#
                  |..#.##.#.
                  |..##..##.
                  |#.#.##.#.""".stripMargin

    MirrorFinder.parse(input) shouldBe 5
  }

}
