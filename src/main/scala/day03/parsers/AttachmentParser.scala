package day03.parsers

import day03.misc.{NeighborHoodLocator, NumberLocator, SymbolLocator}
import day03.model.{AttachmentRecord, NumberRecord, Point, Region, SymbolRecord}
import utils.ContentParser

object AttachmentParser extends ContentParser[Set[AttachmentRecord]] {

  private implicit class PointExtension(p: Point) {
    def within(region: Region): Boolean = {
      p.x >= region.topLeft.x &&
        p.x <= region.bottomRight.x &&
        p.y >= region.topLeft.y &&
        p.y <= region.bottomRight.y
    }
  }

  override def parse(content: String): Set[AttachmentRecord] = {
    val matrix: List[List[Char]] = content.split("\n").map(_.toList).toList

    val numbers: Set[NumberRecord] = NumberLocator.locateNumbers(content)
    val numberNeighborhoods: Map[NumberRecord, Region] =
      numbers.map(num => num -> NeighborHoodLocator.locateNeighborhood(num.area)).toMap
    val symbols: Set[SymbolRecord] = SymbolLocator.locateSymbols(content)


    symbols.map { symbol =>
      val attachedNumbers = numbers.filter(num => symbol.location.within(numberNeighborhoods(num)))
      AttachmentRecord(symbol.shape, attachedNumbers.toList.map(_.value))
    }

  }
}
