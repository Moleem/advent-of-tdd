package day10.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PipeMazeParserSpec extends AnyFlatSpec with Matchers {

  behavior of PipeMazeParser.getClass.getName

  it should "correctly parse a simple map" in {
    val input = """..F7.
                  |.FJ|.
                  |SJ.L7
                  ||F--J
                  |LJ...""".stripMargin
    val expectedOutput = List(
      List('.', '.', 'F', '7', '.'),
      List('.', 'F', 'J', '|', '.'),
      List('S', 'J', '.', 'L', '7'),
      List('|', 'F', '-', '-', 'J'),
      List('L', 'J', '.', '.', '.'),
    )

    PipeMazeParser.parse(input) shouldBe expectedOutput
  }

}
