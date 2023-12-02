package day02.task1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/*
You play several games and record the information from each game (your puzzle input). Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated list of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).

For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
In game 1, three sets of cubes are revealed from the bag (and then put back again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

The Elf would first like to know which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with that configuration. However, game 3 would have been impossible because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you get 8.

Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?
 */

case class Game(id: Int, draws: Seq[Map[String, Int]])
case class GameState(games: Seq[Game])

class SolutionDraft(content: String) {
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

    new SolutionDraft(content).getParsedState shouldBe expectedState
  }

  it should "identify a possible round" in {
    val content = "Game 1: 12 red; 13 green; 14 blue"

    new SolutionDraft(content).sumPossible shouldBe 1
  }

  it should "identify an impossible round" in {
    val content = "Game 1: 13 red; 14 green; 15 blue"

    new SolutionDraft(content).sumPossible shouldBe 0
  }

  it should "identify an impossible round (due to red)" in {
    val content = "Game 1: 13 red; 1 green; 1 blue"

    new SolutionDraft(content).sumPossible shouldBe 0
  }

  it should "identify an impossible round (due to green)" in {
    val content = "Game 1: 1 red; 14 green; 1 blue"

    new SolutionDraft(content).sumPossible shouldBe 0
  }

  it should "identify an impossible round (due to blue)" in {
    val content = "Game 1: 1 red; 15 green; 15 blue"

    new SolutionDraft(content).sumPossible shouldBe 0
  }

  it should "sum game IDs of possible rounds" in {
    val content =
      """Game 1: 11 red; 12 green; 13 blue
        |Game 2: 12 red; 13 green; 14 blue
        |Game 3: 13 red; 14 green; 15 blue""".stripMargin

    new SolutionDraft(content).sumPossible shouldBe 3 // 1+2
  }

}
