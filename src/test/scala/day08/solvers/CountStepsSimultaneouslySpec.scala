package day08.solvers

import day08.model.MovementMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CountStepsSimultaneouslySpec extends AnyFlatSpec with Matchers {

  behavior of CountStepsSimultaneously.getClass.getName

  it should "find step count if the target is immediate (left)" in {
    val input = MovementMap(
      directions = List('L'), mappings = Map(
        "11A" -> ("11Z", "___"),
        "11Z" -> ("11Z", "___"),
        "22A" -> ("22Z", "___"),
        "22Z" -> ("22Z", "___")
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 1
  }

  it should "find step count if the target is immediate (right)" in {
    val input = MovementMap(
      directions = List('R'), mappings = Map(
        "11A" -> ("___", "11Z"),
        "11Z" -> ("___", "11Z"),
        "22A" -> ("___", "22Z"),
        "22Z" -> ("___", "22Z")
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 1
  }

  it should "find step count if the target is within one loop" in {
    val input = MovementMap(
      directions = List('L', 'R'), mappings = Map(
        "11A" -> ("11B", "___"),
        "11B" -> ("___", "11Z"),
        "11Z" -> ("11Z", "11Z"),
        "22A" -> ("22B", "___"),
        "22B" -> ("___", "22Z"),
        "22Z" -> ("22Z", "22Z"),
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 2
  }


  it should "find step count if the target is beyond one loop" in {
    val input = MovementMap(
      directions = List('L', 'R'), mappings = Map(
        "11A" -> ("11B", "___"),
        "11B" -> ("___", "11C"),
        "11C" -> ("11Z", "___"),
        "11Z" -> ("11Z", "11Z"),
        "22A" -> ("22B", "___"),
        "22B" -> ("___", "22C"),
        "22C" -> ("22Z", "___"),
        "22Z" -> ("22Z", "22Z")
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 3
  }

  it should "find step count for targets with different repeat cycles (quicker)" in {
    val input: MovementMap = MovementMap(
      directions = List('L'),
      mappings = Map(
        "11A" -> ("11Z", "___"),
        "11Z" -> ("11Z", "___"),
        "22A" -> ("22B", "___"),
        "22B" -> ("22C", "___"),
        "22C" -> ("22Z", "___"),
        "22Z" -> ("22Z", "___"),
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 3
  }

  it should "find step count for targets with different repeat cycles (longer)" in {
    val input: MovementMap = MovementMap(
      directions = List('L'),
      mappings = Map(
        "11A" -> ("11B", "___"),
        "11B" -> ("11C", "___"),
        "11C" -> ("11D", "___"),
        "11D" -> ("11E", "___"),
        "11E" -> ("11Z", "___"),
        "11Z" -> ("11F", "___"), // <- first Z and cycle start
        "11F" -> ("11G", "___"),
        "11G" -> ("11H", "___"),
        "11H" -> ("11I", "___"),
        "11I" -> ("11Z", "___"), // <- second Z and cycle end
        "22A" -> ("22B", "___"),
        "22B" -> ("22C", "___"),
        "22C" -> ("22D", "___"),
        "22D" -> ("22E", "___"),
        "22E" -> ("22F", "___"),
        "22F" -> ("22G", "___"),
        "22G" -> ("22Z", "___"), // <- first Z and cycle start
        "22Z" -> ("22H", "___"),
        "22H" -> ("22I", "___"),
        "22I" -> ("22J", "___"),
        "22J" -> ("22K", "___"),
        "22K" -> ("22L", "___"),
        "22L" -> ("22M", "___"),
        "22M" -> ("22Z", "___"),
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 35
  }

  it should "solve the official example" in {
    // LR
    //
    //11A = (11B, XXX)
    //11B = (XXX, 11Z)
    //11Z = (11B, XXX)
    //22A = (22B, XXX)
    //22B = (22C, 22C)
    //22C = (22Z, 22Z)
    //22Z = (22B, 22B)
    //XXX = (XXX, XXX)
    val input: MovementMap = MovementMap(
      directions = List('L', 'R'),
      mappings = Map(
        "11A" -> ("11B", "XXX"),
        "11B" -> ("XXX", "11Z"),
        "11Z" -> ("11B", "XXX"),
        "22A" -> ("22B", "XXX"),
        "22B" -> ("22C", "22C"),
        "22C" -> ("22Z", "22Z"),
        "22Z" -> ("22B", "22B"),
        "XXX" -> ("XXX", "XXX")
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 6
  }
}
