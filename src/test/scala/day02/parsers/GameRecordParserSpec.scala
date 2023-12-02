package day02.parsers

import day02.model._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks


class GameRecordParserSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "GameRecordParser"

  it should "be able to parse one color per draw in a single round" in {
    val input = "Game 1: 1 red; 1 green; 1 blue"
    val expectedOutput = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 1),
          Map("green" -> 1),
          Map("blue" -> 1)
        )
      )
    )

    GameRecordParser.parse(input) shouldBe expectedOutput
  }

  it should "be able to parse multiple colors per draw in a single round" in {
    val input = "Game 1: 1 red, 2 green, 3 blue; 1 green, 2 blue; 1 blue"
    val expectedOutput = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 1, "green" -> 2, "blue" -> 3),
          Map("green" -> 1, "blue" -> 2),
          Map("blue" -> 1)
        )
      )
    )

    GameRecordParser.parse(input) shouldBe expectedOutput
  }

  it should "be able to parse multiple rounds" in {
    val input =
      """Game 1: 1 red; 1 green; 1 blue
        |Game 2: 1 red; 1 green; 1 blue""".stripMargin
    val expectedOutput = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("red" -> 1),
          Map("green" -> 1),
          Map("blue" -> 1)
        )
      ),
      GameRecord(
        id = 2,
        rounds = Seq(
          Map("red" -> 1),
          Map("green" -> 1),
          Map("blue" -> 1)
        )
      )
    )

    GameRecordParser.parse(input) shouldBe expectedOutput
  }

  it should "be able to parse example" in {
    val input =
      """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin
    val expectedOutput = Seq(
      GameRecord(
        id = 1,
        rounds = Seq(
          Map("blue" -> 3, "red" -> 4),
          Map("red" -> 1, "green" -> 2, "blue" -> 6),
          Map("green" -> 2)
        )
      ),
      GameRecord(
        id = 2,
        rounds = Seq(
          Map("blue" -> 1, "green" -> 2),
          Map("green" -> 3, "blue" -> 4, "red" -> 1),
          Map("green" -> 1, "blue" -> 1)
        )
      ),
      GameRecord(
        id = 3,
        rounds = Seq(
          Map("green" -> 8, "blue" -> 6, "red" -> 20),
          Map("blue" -> 5, "red" -> 4, "green" -> 13),
          Map("green" -> 5, "red" -> 1)
        )
      ),
      GameRecord(
        id = 4,
        rounds = Seq(
          Map("green" -> 1, "red" -> 3, "blue" -> 6),
          Map("green" -> 3, "red" -> 6),
          Map("green" -> 3, "blue" -> 15, "red" -> 14)
        )
      ),
      GameRecord(
        id = 5,
        rounds = Seq(
          Map("red" -> 6, "blue" -> 1, "green" -> 3),
          Map("blue" -> 2, "red" -> 1, "green" -> 2)
        )
      )
    )

    GameRecordParser.parse(input) shouldBe expectedOutput
  }

}

