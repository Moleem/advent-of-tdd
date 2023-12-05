package day05.solvers

import day05.model.{MappingRange, Mappings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class FindLowestLocationSpec extends AnyFlatSpec with Matchers {

  behavior of "FindLowestLocation"

  it should "find minimum location correctly if no mapping is defined" in {
    val mappings = Mappings(
      List(1, 11),
      List.empty,
      List.empty,
      List.empty,
      List.empty,
      List.empty,
      List.empty,
      List.empty
    )

    FindLowestLocation.solve(mappings) shouldBe 1
  }

  it should "find location correctly if everything is mapped to itself explicitly" in {
    val mappings = Mappings(
      List(1),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0)),
      List(MappingRange(0, 99, +0))
    )

    FindLowestLocation.solve(mappings) shouldBe 1
  }

  it should "find location correctly through changing mapping" in {
    val mappings = Mappings(
      List(1),
      List(MappingRange(1, 1, +1)),
      List(MappingRange(2, 2, +1)),
      List(MappingRange(3, 3, +1)),
      List(MappingRange(4, 4, +1)),
      List(MappingRange(5, 5, +1)),
      List(MappingRange(6, 6, +1)),
      List(MappingRange(7, 7, +1))
    )

    FindLowestLocation.solve(mappings) shouldBe 8
  }

  it should "find minimum relevant location correctly" in {
    val mappings = Mappings(
      List(0, 1),
      List(MappingRange(1, 1, +1)),
      List(MappingRange(2, 2, +1)),
      List(MappingRange(3, 3, +1)),
      List(MappingRange(4, 4, +1)),
      List(MappingRange(5, 5, +1)),
      List(MappingRange(6, 6, +1)),
      List(MappingRange(7, 7, +1))
    )

    FindLowestLocation.solve(mappings) shouldBe 0
  }

  it should "ignore global minimum, if there it's seed is not interesting to us" in {
    val mappings = Mappings(
      List(1),
      List(MappingRange(0, 0, +0), MappingRange(1, 1, +1)),
      List(MappingRange(0, 0, +0), MappingRange(2, 2, +1)),
      List(MappingRange(0, 0, +0), MappingRange(3, 3, +1)),
      List(MappingRange(0, 0, +0), MappingRange(4, 4, +1)),
      List(MappingRange(0, 0, +0), MappingRange(5, 5, +1)),
      List(MappingRange(0, 0, +0), MappingRange(6, 6, +1)),
      List(MappingRange(0, 0, +0), MappingRange(7, 7, +1))
    )

    FindLowestLocation.solve(mappings) shouldBe 8
  }

}
