package day02.task2

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