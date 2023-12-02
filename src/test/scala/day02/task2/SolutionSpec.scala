package day02.task2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

case class Game(id: Int, draws: Seq[Map[String, Int]])
case class GameState(games: Seq[Game])

class Solution(content: String) {
  private val thresholds = Map("red"->12, "green"->13, "blue"->14)

  private def parseColorDraw(colorDraw: String): (String, Int) = {
    val chunks = colorDraw.trim.split(" ")
    val amount = chunks(0).toInt
    val color = chunks(1).trim
    (color, amount)
  }

  private def parseDraw(draw: String): Map[String, Int] =
    draw.split(",").map(parseColorDraw).toMap

  private def parseRound(round: String): Seq[Map[String, Int]] =
    round.split(";").map(parseDraw).toSeq

  private def parseGameId(gameHeader: String): Int =
    gameHeader.split(" ")(1).toInt

  def getParsedState: GameState = {
    val games = content
      .split("\n")
      .map { line =>
        val lineChunks = line.split(":")
        val gameId = parseGameId(lineChunks(0))
        val rounds = parseRound(lineChunks(1))
        Game(gameId, rounds)
      }.toSeq

    GameState(games)
  }

  private def isPossibleDraw(color: String, amount: Int): Boolean =
    thresholds(color) >= amount

  def sumPossible: Int = {

    val possibleGames = getParsedState.games.filter {
      game =>
        game.draws.forall{ draw =>
          draw.toSeq.forall{ case (color, amount) => isPossibleDraw(color, amount)}
        }
    }

    possibleGames.map(_.id).sum
  }

  def sumPowerMin: Int = {
    getParsedState.games.map { game =>
      val minimumRequirements: Map[String, Int] = game.draws.reduce{ (drawA, drawB) =>
        val allKeys = drawA.keys ++ drawB.keys
        allKeys.map{ key =>
          val countA = drawA.getOrElse(key, 0)
          val countB = drawB.getOrElse(key, 0)

          (key, Seq(countA, countB).max)
        }.toMap
      }

      minimumRequirements.values.product
    }
  }.sum

}


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

  it should "find minimum requirements (all 1)" in {
    val content = "Game 1: 1 red; 1 green; 1 blue"

    new Solution(content).sumPowerMin shouldBe 1
  }

  it should "find minimum requirements (one larger)" in {
    val content = "Game 1: 2 red; 1 green; 1 blue"

    new Solution(content).sumPowerMin shouldBe 2
  }

  it should "find minimum requirements (find max across draws)" in {
    val content = "Game 1: 2 red; 1 red, 1 green; 1 blue"

    new Solution(content).sumPowerMin shouldBe 2
  }

  it should "find minimum power" in {
    val content = "Game 1: 1 red; 2 green; 3 blue"

    new Solution(content).sumPowerMin shouldBe 6
  }

  it should "sum minimum powers" in {
    val content =
      """Game 1: 1 red; 2 green; 3 blue
        |Game 2: 1 red; 2 green; 3 blue""".stripMargin

    new Solution(content).sumPowerMin shouldBe 12
  }

}

