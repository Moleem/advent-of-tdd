package day03.misc

import day03.model.{Point, Region}

object NeighborHoodLocator {
  def locateNeighborhood(region: Region): Region =
    Region(Point(region.topLeft.x - 1, region.topLeft.y - 1), Point(region.bottomRight.x + 1, region.bottomRight.y + 1))
}
