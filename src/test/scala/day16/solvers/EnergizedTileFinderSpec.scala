package day16.solvers

import day16.model.{Right, Left, Up, Down}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class EnergizedTileFinderSpec extends AnyFlatSpec with Matchers {

  behavior of "EnergizedTileFinder"

  it should "work (empty tile, beam travels right)" in {
    val input =
      """.....
        |.....
        |.....""".stripMargin
    val expectedOutput =
      """.....
        |#####
        |.....""".stripMargin

    new EnergizedTileFinder(1, 0, Right).solve(input) shouldBe expectedOutput
  }

  it should "work (empty tile, beam travels left)" in {
    val input =
      """.....
        |.....
        |.....""".stripMargin
    val expectedOutput =
      """.....
        |#####
        |.....""".stripMargin

    new EnergizedTileFinder(1, 4, Left).solve(input) shouldBe expectedOutput
  }

  it should "work (empty tile, beam travels up)" in {
    val input =
      """.....
        |.....
        |.....""".stripMargin
    val expectedOutput =
      """..#..
        |..#..
        |..#..""".stripMargin

    new EnergizedTileFinder(2, 2, Up).solve(input) shouldBe expectedOutput
  }

  it should "work (empty tile, beam travels down)" in {
    val input =
      """.....
        |.....
        |.....""".stripMargin
    val expectedOutput =
      """..#..
        |..#..
        |..#..""".stripMargin

    new EnergizedTileFinder(0, 2, Down).solve(input) shouldBe expectedOutput
  }

  it should "work (horizontal splitter, beam travels right)" in {
    val input =
      """.....
        |..-..
        |.....""".stripMargin

    val expectedOutput =
      """.....
        |#####
        |.....""".stripMargin

    new EnergizedTileFinder(1, 0, Right).solve(input) shouldBe expectedOutput
  }

  it should "work (horizontal splitter, beam travels left)" in {
    val input =
      """.....
        |..-..
        |.....""".stripMargin

    val expectedOutput =
      """.....
        |#####
        |.....""".stripMargin

    new EnergizedTileFinder(1, 4, Left).solve(input) shouldBe expectedOutput
  }

  it should "work (horizontal splitter, beam travels up)" in {
    val input =
      """.....
        |..-..
        |.....""".stripMargin

    val expectedOutput =
      """.....
        |#####
        |..#..""".stripMargin

    new EnergizedTileFinder(2, 2, Up).solve(input) shouldBe expectedOutput
  }

  it should "work (horizontal splitter, beam travels down)" in {
    val input =
      """.....
        |..-..
        |.....""".stripMargin

    val expectedOutput =
      """..#..
        |#####
        |.....""".stripMargin

    new EnergizedTileFinder(0, 2, Down).solve(input) shouldBe expectedOutput
  }

  it should "work (vertical splitter, beam travels right)" in {
    val input =
      """.....
        |..|..
        |.....""".stripMargin

    val expectedOutput =
      """..#..
        |###..
        |..#..""".stripMargin

    new EnergizedTileFinder(1, 0, Right).solve(input) shouldBe expectedOutput
  }

  it should "work (vertical splitter, beam travels left)" in {
    val input =
      """.....
        |..|..
        |.....""".stripMargin

    val expectedOutput =
      """..#..
        |..###
        |..#..""".stripMargin

    new EnergizedTileFinder(1, 4, Left).solve(input) shouldBe expectedOutput
  }

  it should "work (vertical splitter, beam travels up)" in {
    val input =
      """.....
        |..|..
        |.....""".stripMargin

    val expectedOutput =
      """..#..
        |..#..
        |..#..""".stripMargin

    new EnergizedTileFinder(2, 2, Up).solve(input) shouldBe expectedOutput
  }

  it should "work (vertical splitter, beam travels down)" in {
    val input =
      """.....
        |..|..
        |.....""".stripMargin

    val expectedOutput =
      """..#..
        |..#..
        |..#..""".stripMargin

    new EnergizedTileFinder(0, 2, Down).solve(input) shouldBe expectedOutput
  }

  it should "work (down leaning mirror, beam travels right)" in {
    val input =
      """.....
        |..\..
        |.....""".stripMargin

    val expectedOutput =
      """.....
        |###..
        |..#..""".stripMargin

    new EnergizedTileFinder(1, 0, Right).solve(input) shouldBe expectedOutput
  }

  it should "work (down leaning mirror, beam travels left)" in {
    val input =
      """.....
        |..\..
        |.....""".stripMargin

    val expectedOutput =
      """..#..
        |..###
        |.....""".stripMargin

    new EnergizedTileFinder(1, 4, Left).solve(input) shouldBe expectedOutput
  }

  it should "work (down leaning mirror, beam travels up)" in {
    val input =
      """.....
        |..\..
        |.....""".stripMargin

    val expectedOutput =
      """.....
        |###..
        |..#..""".stripMargin

    new EnergizedTileFinder(2, 2, Up).solve(input) shouldBe expectedOutput
  }

  it should "work (down leaning mirror, beam travels down)" in {
    val input =
      """.....
        |..\..
        |.....""".stripMargin

    val expectedOutput =
      """..#..
        |..###
        |.....""".stripMargin

    new EnergizedTileFinder(0, 2, Down).solve(input) shouldBe expectedOutput
  }

  it should "work (up leaning mirror, beam travels right)" in {
    val input =
      """.....
        |../..
        |.....""".stripMargin

    val expectedOutput =
      """..#..
        |###..
        |.....""".stripMargin

    new EnergizedTileFinder(1, 0, Right).solve(input) shouldBe expectedOutput
  }

  it should "work (up leaning mirror, beam travels left)" in {
    val input =
      """.....
        |../..
        |.....""".stripMargin

    val expectedOutput =
      """.....
        |..###
        |..#..""".stripMargin

    new EnergizedTileFinder(1, 4, Left).solve(input) shouldBe expectedOutput
  }

  it should "work (up leaning mirror, beam travels up)" in {
    val input =
      """.....
        |../..
        |.....""".stripMargin

    val expectedOutput =
      """.....
        |..###
        |..#..""".stripMargin

    new EnergizedTileFinder(2, 2, Up).solve(input) shouldBe expectedOutput
  }

  it should "work (up leaning mirror, beam travels down)" in {
    val input =
      """.....
        |../..
        |.....""".stripMargin

    val expectedOutput =
      """..#..
        |###..
        |.....""".stripMargin

    new EnergizedTileFinder(0, 2, Down).solve(input) shouldBe expectedOutput
  }

  it should "work on the example" in {
    val input =
      """.|...\....
        ||.-.\.....
        |.....|-...
        |........|.
        |..........
        |.........\
        |..../.\\..
        |.-.-/..|..
        |.|....-|.\
        |..//.|....""".stripMargin

    val expectedOutput =
      """######....
        |.#...#....
        |.#...#####
        |.#...##...
        |.#...##...
        |.#...##...
        |.#..####..
        |########..
        |.#######..
        |.#...#.#..""".stripMargin

    println(expectedOutput)

    println(new EnergizedTileFinder(0, 0, Right).solve(input))

    new EnergizedTileFinder(0, 0, Right).solve(input) shouldBe expectedOutput
  }

}
