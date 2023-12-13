package day12.solvers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpringErrorMatcherSpec extends AnyFlatSpec with Matchers {

  behavior of SpringErrorMatcher.getClass.getName

  it should "return a single possible arrangement if there are no unknowns" in {
    val input = List((".#.##.###.", List(1, 2, 3)))

    SpringErrorMatcher.solve(input) shouldBe 1
  }

  it should "return a single possible arrangement if all the error positions are known" in {
    val input = List(("?#?##?###?", List(1, 2, 3)))

    SpringErrorMatcher.solve(input) shouldBe 1
  }

  it should "return a single possible arrangement if all the good positions are known" in {
    val input = List((".?.??.???.", List(1, 2, 3)))

    SpringErrorMatcher.solve(input) shouldBe 1
  }

  it should "return a single possible arrangement if there is only one possible arrangement" in {
    val input = List((".?.??.???.?.?.?.?", List(1, 2, 3)))

    SpringErrorMatcher.solve(input) shouldBe 1
  }

  it should "return two possible arrangements if the error can go in two different places (1)" in {
    val input = List((".??.", List(1)))

    SpringErrorMatcher.solve(input) shouldBe 2
  }

  it should "return two possible arrangements if the error can go in two different places (2)" in {
    val input = List((".?.?.", List(1)))

    SpringErrorMatcher.solve(input) shouldBe 2
  }

  it should "return two possible arrangements if the error can go in two different places (3)" in {
    val input = List((".??.??.", List(1, 2)))

    SpringErrorMatcher.solve(input) shouldBe 2
  }

  it should "return four possible arrangements if the error can go in four different places (1)" in {
    val input = List((".??.??.", List(1, 1)))

    SpringErrorMatcher.solve(input) shouldBe 4
  }

  it should "work correctly with example input (1)" in {
    val input = List(("???.###", List(1,1,3)))

    SpringErrorMatcher.solve(input) shouldBe 1
  }

  it should "work correctly with example input (2)" in {
    val input = List((".??..??...?##.", List(1,1,3)))

    SpringErrorMatcher.solve(input) shouldBe 4
  }

  it should "work correctly with example input (3)" in {
    val input = List(("?#?#?#?#?#?#?#?", List(1,3,1,6)))

    SpringErrorMatcher.solve(input) shouldBe 1
  }

  it should "work correctly with example input (4)" in {
    val input = List(("????.#...#...", List(4,1,1)))

    SpringErrorMatcher.solve(input) shouldBe 1
  }

  it should "work correctly with example input (5)" in {
    val input = List(("????.######..#####.", List(1,6,5)))

    SpringErrorMatcher.solve(input) shouldBe 4
  }

  it should "work correctly with example input (6)" in {
    val input = List(("?###????????", List(3,2,1)))

    SpringErrorMatcher.solve(input) shouldBe 10
  }
}
