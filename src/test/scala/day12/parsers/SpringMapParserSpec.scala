package day12.parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpringMapParserSpec extends AnyFlatSpec with Matchers {

  behavior of SpringMapParser.getClass.getName

  it should "parse the input correctly" in {
    val input = """???.### 1,1,3
                  |.??..??...?##. 1,1,3
                  |?#?#?#?#?#?#?#? 1,3,1,6
                  |????.#...#... 4,1,1
                  |????.######..#####. 1,6,5
                  |?###???????? 3,2,1""".stripMargin

    val expectedOutput = List(
      ("???.###", List(1,1,3)),
      (".??..??...?##.", List( 1,1,3)),
      ("?#?#?#?#?#?#?#?", List( 1,3,1,6)),
      ("????.#...#...", List( 4,1,1)),
      ("????.######..#####.", List( 1,6,5)),
      ("?###????????", List( 3,2,1))
    )

    SpringMapParser.parse(input) shouldBe expectedOutput
  }
}
