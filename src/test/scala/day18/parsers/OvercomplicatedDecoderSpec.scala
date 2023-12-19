package day18.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OvercomplicatedDecoderSpec extends AnyFlatSpec with Matchers {

  behavior of OvercomplicatedDecoder.getClass.getSimpleName

  it should "decode both distance and color" in {
    val input = """L 1 (#70c710)
                  |L 1 (#0dc571)
                  |L 1 (#5713f0)
                  |L 1 (#d2c081)
                  |L 1 (#59c680)
                  |L 1 (#411b91)
                  |L 1 (#8ceee2)
                  |L 1 (#caa173)
                  |L 1 (#1b58a2)
                  |L 1 (#caa171)
                  |L 1 (#7807d2)
                  |L 1 (#a77fa3)
                  |L 1 (#015232)
                  |L 1 (#7a21e3)""".stripMargin

    val expectedOutput = List(
      List('R', 461937),
      List('D', 56407),
      List('R', 356671),
      List('D', 863240),
      List('R', 367720),
      List('D', 266681),
      List('L', 577262),
      List('U', 829975),
      List('L', 112010),
      List('D', 829975),
      List('L', 491645),
      List('U', 686074),
      List('L', 5411),
      List('U', 500254)
    )

    OvercomplicatedDecoder.parse(input) shouldBe expectedOutput
  }

}
