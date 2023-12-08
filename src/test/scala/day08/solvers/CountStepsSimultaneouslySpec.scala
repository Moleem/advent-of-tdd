package day08.solvers

import day08.model.MovementMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.ProblemSolver

import scala.annotation.tailrec

object CountStepsSimultaneously extends ProblemSolver[MovementMap, Int] {
  override def solve(input: MovementMap): Int =
    countSteps(input, input.mappings.keySet.filter(_.endsWith("A")))


  @tailrec
  private def countSteps(
                          input: MovementMap,
                          keys: Set[String],
                          stepCount: Int = 0
                        ): Int = {
    if (keys.forall(_.endsWith("Z"))) stepCount
    else {
      val directionIndex = stepCount % input.directions.size
      val direction = input.directions(directionIndex)
      val nextKeys = keys.map(key =>
        direction match {
          case 'L' => input.mappings(key)._1
          case 'R' => input.mappings(key)._2
        })
      countSteps(input, nextKeys, stepCount + 1)
    }
  }


  //  {
  //    val keysEndingWithA = input.mappings.keys.filter(_.endsWith("A"))
  //
  //    input.directions.head match {
  //      case 'L' =>
  //        if (keysEndingWithA.forall(key => input.mappings(key)._1.endsWith("Z"))) 1 else ???
  //      case 'R' =>
  //        if (keysEndingWithA.forall(key => input.mappings(key)._2.endsWith("Z"))) 1 else ???
  //    }
  //  }
}

class CountStepsSimultaneouslySpec extends AnyFlatSpec with Matchers {

  behavior of CountStepsSimultaneously.getClass.getName

  it should "find step count if the target is immediate (left)" in {
    val input = MovementMap(
      directions = List('L'), mappings = Map(
        "11A" -> ("11Z", "___"),
        "22A" -> ("22Z", "___")
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 1
  }

  it should "find step count if the target is immediate (right)" in {
    val input = MovementMap(
      directions = List('R'), mappings = Map(
        "11A" -> ("___", "11Z"),
        "22A" -> ("___", "22Z")
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 1
  }

  it should "find step count if the target is within one loop" in {
    val input = MovementMap(
      directions = List('L', 'L'), mappings = Map(
        "11A" -> ("11B", "___"),
        "22A" -> ("22B", "___"),
        "11B" -> ("11Z", "___"),
        "22B" -> ("22Z", "___"),
      )
    )

    CountStepsSimultaneously.solve(input) shouldBe 2
  }

}
