//package day05.solvers
//
//import day05.model.Mappings
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//
//
//class FindLowestLocationSpec extends AnyFlatSpec with Matchers {
//
//  behavior of "FindLowestLocation"
//
//  it should "find location correctly" in {
//    val mappings = Mappings(
//      List(1),
//      Map(1 -> 2),
//      Map(2 -> 3),
//      Map(3 -> 4),
//      Map(4 -> 5),
//      Map(5 -> 6),
//      Map(6 -> 7),
//      Map(7 -> 8)
//    )
//
//    FindLowestLocation.solve(mappings) shouldBe 8
//  }
//
//  it should "find minimum location correctly" in {
//    val mappings = Mappings(
//      List(1, 11),
//      Map(1 -> 2, 11 -> 22),
//      Map(2 -> 3, 22 -> 33),
//      Map(3 -> 4, 33 -> 44),
//      Map(4 -> 5, 44 -> 55),
//      Map(5 -> 6, 55 -> 66),
//      Map(6 -> 7, 66 -> 77),
//      Map(7 -> 8, 77 -> 88)
//    )
//
//    FindLowestLocation.solve(mappings) shouldBe 8
//  }
//
//  it should "ignore global minimum, if there it's seed is not interesting to us" in {
//    val mappings = Mappings(
//      List(1, 11),
//      Map(0 -> 0, 1 -> 2, 11 -> 22),
//      Map(0 -> 0, 2 -> 3, 22 -> 33),
//      Map(0 -> 0, 3 -> 4, 33 -> 44),
//      Map(0 -> 0, 4 -> 5, 44 -> 55),
//      Map(0 -> 0, 5 -> 6, 55 -> 66),
//      Map(0 -> 0, 6 -> 7, 66 -> 77),
//      Map(0 -> 0, 7 -> 8, 77 -> 88)
//    )
//
//    FindLowestLocation.solve(mappings) shouldBe 8
//  }
//
//}
