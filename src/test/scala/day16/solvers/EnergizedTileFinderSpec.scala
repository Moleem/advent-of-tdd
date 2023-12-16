package day16.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

sealed trait Direction
case object Right extends Direction

class EnergizedTileFinder(startRow: Int, startCol: Int, direction: Direction) extends ProblemSolver[String, String] {
  override def solve(input: String): String = ???
}

class EnergizedTileFinderSpec extends AnyFlatSpec with Matchers {

  behavior of "EnergizedTileFinder"

  it should "work (empty tile, beam travels right)" in {
    val input =
      """.....
        |.....
        |.....""".stripMargin
    val expectedOutput =
      """.....
        |.....
        |.....""".stripMargin

    new EnergizedTileFinder(1,0,Right).solve(input) shouldBe expectedOutput
  }

}
