package day01.parsers

import day01.model.Calibration
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class NaiveCalibrationParserSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "NaiveCalibrationParser"

  private val failureScenarios = Table(
    ("scenario name",                       "input"              ),
    ("empty content",                       ""                   ),
    ("content without digits",              "aaa"                ),
    ("content without digits in each line", """1a1
                                              |bbb""".stripMargin)
  )

  forAll(failureScenarios) { (scenarioName: String, input: String) =>

    it should s"fail in scenario [$scenarioName]" in {
      an[IllegalStateException] should be thrownBy NaiveCalibrationParser.parse(input)
    }

  }


  private val normalScenarios = Table(
    ("scenario name",                                  "expected output",      "input"                     ),
    ("two digit number",                                Seq(Calibration(2,5)), "25"                        ),
    ("two digit number with letters inbetween",         Seq(Calibration(2,5)), "2a5"                       ),
    ("two digit number nested",                         Seq(Calibration(2,5)), "x25x"                      ),
    ("two digit number nested with letters inbetween",  Seq(Calibration(2,5)), "2x5"                       ),
    ("two digit number nested with numbers inbetween",  Seq(Calibration(2,5)), "205"                       ),
    ("one digit number",                                Seq(Calibration(5,5)), "5"                         ),
    ("one digit number with letters",                   Seq(Calibration(5,5)), "x5x"                       ),
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
      NaiveCalibrationParser.parse(input) shouldBe expectedOutput
    }

  }

}

