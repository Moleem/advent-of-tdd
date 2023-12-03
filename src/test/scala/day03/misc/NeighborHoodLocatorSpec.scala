package day03.misc


import day03.model._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object NeighborHoodLocator {
  def locateNeighborhood(region: Region): Region =
    Region(Point(region.topLeft.x-1, region.topLeft.y-1), Point(region.bottomRight.x+1, region.bottomRight.y+1))
}

class NeighborHoodLocatorSpec extends AnyFlatSpec with Matchers {

  behavior of "NeighborHoodLocator"

  it should "locate the neighborhood around a single point region" in {
    val region = Region(Point(0, 0), Point(0, 0))
    val expectedNeighborhood = Region(Point(-1, -1), Point(1, 1))
    NeighborHoodLocator.locateNeighborhood(region) shouldBe expectedNeighborhood
  }

  it should "locate the neighborhood around a single line region" in {
    val region = Region(Point(0, 0), Point(0, 3))
    val expectedNeighborhood = Region(Point(-1, -1), Point(1, 4))
    NeighborHoodLocator.locateNeighborhood(region) shouldBe expectedNeighborhood
  }

  it should "locate the neighborhood around a block region" in {
    val region = Region(Point(0, 0), Point(2, 3))
    val expectedNeighborhood = Region(Point(-1, -1), Point(3, 4))
    NeighborHoodLocator.locateNeighborhood(region) shouldBe expectedNeighborhood
  }

}
