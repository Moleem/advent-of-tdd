package day18.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SmarterAreaCounterSpec extends AnyFlatSpec with Matchers {

  behavior of SmarterAreaCounter.getClass.getSimpleName

  it should "work on simple rectangles" in {
    val input = List(
      ('R', 2),
      ('D', 1),
      ('L', 2),
      ('U', 1)
    )
    // ###
    // ###
    SmarterAreaCounter.solve(input) shouldBe 6
  }

  it should "work on bottom left reduction" in {
    val input = List(
      ('R', 4),
      ('D', 3),
      ('L', 2),
      ('U', 2),
      ('L', 2),
      ('U', 1)
    )
    // #####
    // #####
    //   ###
    //   ###
    SmarterAreaCounter.solve(input) shouldBe 16
  }

  it should "work on bottom right reduction" in {
    val input = List(
      ('R', 4),
      ('D', 1),
      ('L', 2),
      ('D', 2),
      ('L', 2),
      ('U', 3)
    )
    // #####
    // #####
    // ###
    // ###
    SmarterAreaCounter.solve(input) shouldBe 16
  }

  it should "work on bottom left expansion" in {
    val input = List(
      ('R', 2),
      ('D', 3),
      ('L', 4),
      ('U', 1),
      ('R', 2),
      ('U', 2)
    )
    //   ###
    //   ###
    // #####
    // #####
    SmarterAreaCounter.solve(input) shouldBe 16
  }

  it should "work on bottom right expansion" in {
    val input = List(
      ('R', 2),
      ('D', 2),
      ('R', 2),
      ('D', 1),
      ('L', 4),
      ('U', 3)
    )
    // ###
    // ###
    // #####
    // #####
    SmarterAreaCounter.solve(input) shouldBe 16
  }

  it should "work on bottom reduction" in {
    val input = List(
      ('R', 4),
      ('D', 1),
      ('L', 1),
      ('D', 2),
      ('L', 2),
      ('U', 2),
      ('L', 1),
      ('U', 1)
    )
    // #####
    // #####
    //  ###
    //  ###
    SmarterAreaCounter.solve(input) shouldBe 16
  }

  it should "work on bottom expansion" in {
    val input = List(
      ('R', 2),
      ('D', 2),
      ('R', 1),
      ('D', 1),
      ('L', 4),
      ('U', 1),
      ('R', 1),
      ('U', 2)
    )
    //  ###
    //  ###
    // #####
    // #####
    SmarterAreaCounter.solve(input) shouldBe 16
  }

  it should "work on interval split" in {
    val input = List(
      ('R', 4),
      ('D', 4),
      ('L', 1),
      ('U', 3),
      ('L', 2),
      ('D', 3),
      ('L', 1),
      ('U', 4)
    )
    // #####
    // #####
    // ## ##
    // ## ##
    // ## ##
    SmarterAreaCounter.solve(input) shouldBe 16
  }

  it should "work on interval merge" in {
    val input = List(
      ('R', 1),
      ('D', 3),
      ('R', 2),
      ('U', 3),
      ('R', 1),
      ('D', 4),
      ('L', 4),
      ('U', 4)
    )
    // ## ##
    // ## ##
    // ## ##
    // #####
    // #####
    SmarterAreaCounter.solve(input) shouldBe 16
  }

  it should "work on the example" in {
    val input = List(
      ('R', 6),
      ('D', 5),
      ('L', 2),
      ('D', 2),
      ('R', 2),
      ('D', 2),
      ('L', 5),
      ('U', 2),
      ('L', 1),
      ('U', 2),
      ('R', 2),
      ('U', 3),
      ('L', 2),
      ('U', 2)
    )
    // #######
    // #######
    // #######
    // ..#####
    // ..#####
    // #######
    // #####..
    // #######
    // .######
    // .######
    SmarterAreaCounter.solve(input) shouldBe 62
  }

}
