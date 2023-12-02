package day01.parsers

import day01.model.Calibration
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks


class SpellingAwareCalibrationParserSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "SpellingAwareCalibrationParser"

  private val failureScenarios = Table(
    ("scenario name",                       "input"              ),
    ("empty content",                       ""                   ),
    ("content without digits",              "aaa"                ),
    ("content without digits in each line", """1a1
                                              |bbb""".stripMargin)
  )

  forAll(failureScenarios) { (scenarioName: String, input: String) =>

    it should s"fail in scenario [$scenarioName]" in {
      an[IllegalStateException] should be thrownBy SpellingAwareCalibrationParser.parse(input)
    }

  }


  private val normalScenarios = Table(
    ("scenario name",                                  "expected output",      "input"                     ),
    ("two digit number",                                Seq(Calibration(2,5)), "25"                        ),
    ("two digit number with letters inbetween",         Seq(Calibration(2,5)), "2a5"                       ),
    ("two digit number with digits inbetween",          Seq(Calibration(2,5)), "205"                       ),
    ("two digit number nested",                         Seq(Calibration(2,5)), "x25x"                      ),
    ("two digit number nested with letters inbetween",  Seq(Calibration(2,5)), "x2x5x"                     ),
    ("two digit number nested with digits inbetween",   Seq(Calibration(2,5)), "x205x"                     ),
    ("one digit number",                                Seq(Calibration(5,5)), "5"                         ),
    ("one digit number nested",                         Seq(Calibration(5,5)), "x5x"                       ),
    ("multi line simple",                               Seq(Calibration(2,5),
                                                            Calibration(2,5)), """25
                                                                                 |25""".stripMargin        ),
    ("multi line example",                              Seq(Calibration(1,2),
                                                            Calibration(3,8),
                                                            Calibration(1,5),
                                                            Calibration(7,7)), """1abc2
                                                                                 |pqr3stu8vwx
                                                                                 |a1b2c3d4e5f
                                                                                 |treb7uchet""".stripMargin)
  )

  forAll(normalScenarios) { (scenarioName: String, expectedOutput: Seq[Calibration], input: String) =>

    it should s"work correctly in scenario [$scenarioName]" in {
      SpellingAwareCalibrationParser.parse(input) shouldBe expectedOutput
    }

  }

  private val spellingAwareScenarios = Table(
    ("scenario name",                                            "expected output",      "input"                        ),
    ("(spelled) two digit number",                               Seq(Calibration(2, 5)), "twofive"                      ),
    ("(spelled) two digit number with letters inbetween",        Seq(Calibration(2, 5)), "twoXfive"                     ),
    ("(spelled) two digit number with digits inbetween",         Seq(Calibration(2, 5)), "two0five"                     ),
    ("(spelled) two digit number nested",                        Seq(Calibration(2, 5)), "XtwofiveX"                    ),
    ("(spelled) two digit number nested with letters inbetween", Seq(Calibration(2, 5)), "XtwoXfiveX"                   ),
    ("(spelled) two digit number nested with digits inbetween",  Seq(Calibration(2, 5)), "Xtwo0fiveX"                   ),
    ("(spelled) one digit number",                               Seq(Calibration(5, 5)), "five"                         ),
    ("(spelled) one digit number nested",                        Seq(Calibration(5, 5)), "XfiveX"                       ),
    ("(spelled) multi line simple",                              Seq(Calibration(2, 5),
                                                                     Calibration(2, 5)), """twofive
                                                                                           |25""".stripMargin           ),
    ("(spelled) multi line example",                             Seq(Calibration(2, 9),
                                                                     Calibration(8, 3),
                                                                     Calibration(1, 3),
                                                                     Calibration(2, 4),
                                                                     Calibration(4, 2),
                                                                     Calibration(1, 4),
                                                                     Calibration(7, 6)), """two1nine
                                                                                           |eightwothree
                                                                                           |abcone2threexyz
                                                                                           |xtwone3four
                                                                                           |4nineeightseven2
                                                                                           |zoneight234
                                                                                           |7pqrstsixteen""".stripMargin),
    ("find single if one merged with eight",                     Seq(Calibration(1, 8)), "oneight"                      ),
    ("find single if two merged with one",                       Seq(Calibration(2, 1)), "twone"                        ),
    ("find single if three merged with eight",                   Seq(Calibration(3, 8)), "threeight"                    ),
    ("find single if five merged with eight",                    Seq(Calibration(5, 8)), "fiveight"                     ),
    ("find single if seven merged with nine",                    Seq(Calibration(7, 9)), "sevenine"                     ),
    ("find single if eight merged with two",                     Seq(Calibration(8, 2)), "eightwo"                      ),
    ("find single if eight merged with three",                   Seq(Calibration(8, 3)), "eighthree"                    )
  )


  forAll(spellingAwareScenarios) { (scenarioName: String, expectedOutput: Seq[Calibration], input: String) =>

    it should s"work correctly in scenario [$scenarioName]" in {
      SpellingAwareCalibrationParser.parse(input) shouldBe expectedOutput
    }

  }

}

