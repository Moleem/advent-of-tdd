package day00.solvers

import day00.model.DummyRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DummyTask1SolverSpec extends AnyFlatSpec with Matchers {

  behavior of "DummyTask1Solver"

  it should "return the value of a single element sequence" in {
    DummyTask1Solver.solve(Seq(DummyRecord("hello"))) shouldBe 5
  }

  it should "return sum the values of multi element sequences" in {
    DummyTask1Solver.solve(Seq(DummyRecord("hello"), DummyRecord("there"))) shouldBe 10
  }

}

