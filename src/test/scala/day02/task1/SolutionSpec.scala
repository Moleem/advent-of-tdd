package day02.task1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class SolutionSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "Solution"

  it should "be able to parse the content correctly" in {
    val content = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                    |Game 2: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red""".stripMargin

    val expectedState = GameState(
      Seq(
        Game(1,
          Seq(
            Map("blue" -> 3, "red" -> 4),
            Map("red" -> 1, "green" -> 2, "blue" -> 6),
            Map("green" -> 2)
          )
        ),
        Game(2,
          Seq(
            Map("green" -> 1, "red" -> 3, "blue"-> 6),
            Map("green" -> 3, "red" -> 6),
            Map("green" -> 3, "blue"->15, "red" ->14)
          )
        )
      )
    )

    new Solution(content).getParsedState shouldBe expectedState
  }

  it should "identify a possible round" in {
    val content = "Game 1: 12 red; 13 green; 14 blue"

    new Solution(content).sumPossible shouldBe 1
  }

  it should "identify an impossible round" in {
    val content = "Game 1: 13 red; 14 green; 15 blue"

    new Solution(content).sumPossible shouldBe 0
  }

  it should "identify an impossible round (due to red)" in {
    val content = "Game 1: 13 red; 1 green; 1 blue"

    new Solution(content).sumPossible shouldBe 0
  }

  it should "identify an impossible round (due to green)" in {
    val content = "Game 1: 1 red; 14 green; 1 blue"

    new Solution(content).sumPossible shouldBe 0
  }

  it should "identify an impossible round (due to blue)" in {
    val content = "Game 1: 1 red; 15 green; 15 blue"

    new Solution(content).sumPossible shouldBe 0
  }

  it should "sum game IDs of possible rounds" in {
    val content =
      """Game 1: 11 red; 12 green; 13 blue
        |Game 2: 12 red; 13 green; 14 blue
        |Game 3: 13 red; 14 green; 15 blue""".stripMargin

    new Solution(content).sumPossible shouldBe 3 // 1+2
  }

}

