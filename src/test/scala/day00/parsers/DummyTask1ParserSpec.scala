package day00.parsers

import day00.model.DummyRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks


class DummyTask1ParserSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "DummyTask1Parser"

  private val normalScenarios = Table(
    ("scenario name",                 "expected output",          "input"                ),
    ("single line scenario template",  Seq(DummyRecord("hello")), "hello"                ),
    ("multi line scenario template",   Seq(DummyRecord("line1"),
                                           DummyRecord("line2")), """line1
                                                                    |line2""".stripMargin)
  )

  forAll(normalScenarios) { (scenarioName: String, expectedOutput: Seq[DummyRecord], input: String) =>

    it should s"work correctly in scenario [$scenarioName]" in {
      DummyTask1Parser.parse(input) shouldBe expectedOutput
    }

  }

}

