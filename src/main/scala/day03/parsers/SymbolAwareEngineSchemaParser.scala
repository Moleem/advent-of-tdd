package day03.parsers

import utils.ContentParser

import scala.annotation.tailrec

object SymbolAwareEngineSchemaParser extends ContentParser[Seq[Int]] {

  @tailrec
  private def getNumbers(engineVector: List[Char], digitsAccumulator: Option[Int], maskVector: List[Boolean], symbolAccumulator: Boolean, numsAccumulator: List[Int]): Seq[Int] = {
    (engineVector, digitsAccumulator, maskVector, symbolAccumulator) match {
      case (Nil, None, Nil, _) =>
        numsAccumulator
      case (Nil, Some(_), Nil, false) =>
        numsAccumulator
      case (Nil, Some(num), Nil, true) =>
        num :: numsAccumulator
      case (head :: tail, None, sHead :: sTail, _) if !head.isDigit =>
        getNumbers(tail, digitsAccumulator, sTail, false, numsAccumulator)
      case (head :: tail, None, sHead :: sTail, _) if head.isDigit =>
        getNumbers(tail, Some(Character.getNumericValue(head)), sTail, sHead, numsAccumulator)
      case (head :: tail, Some(num), sHead :: sTail, false) if !head.isDigit =>
        getNumbers(tail, None, sTail, false, numsAccumulator)
      case (head :: tail, Some(num), sHead :: sTail, true) if !head.isDigit =>
        getNumbers(tail, None, sTail, false, num :: numsAccumulator)
      case (head :: tail, Some(num), sHead :: sTail, hasSymbol) if head.isDigit =>
        getNumbers(tail, Some(num * 10 + Character.getNumericValue(head)), sTail, hasSymbol || sHead, numsAccumulator)
    }
  }

  private def isSymbol(c: Char): Boolean = !(c.isDigit || c == '.')

  private def getNeighbors(row: Int, col: Int, rowCount: Int, colCount: Int): Seq[(Int, Int)] = {
    val allNeighbors = Seq(
      (row-1, col-1), (row-1, col), (row-1, col+1),
      (row  , col-1), (row  , col), (row  , col+1),
      (row+1, col-1), (row+1, col), (row+1, col+1)
    )

    val validNeighbors = allNeighbors.filter { case (row, col) =>
      row >= 0 && row < rowCount && col >= 0 && col < colCount
    }

    validNeighbors
  }

  override def parse(content: String): Seq[Int] = {
    val engineMatrix: Seq[Seq[Char]] = content.split("\n").toSeq.map(_.toSeq)
    val rowCount: Int = engineMatrix.length
    val colCount: Int = engineMatrix.head.length

    val symbolNeighborMask: Seq[Seq[Char]] =
      (0 until rowCount).map(row =>
        (0 until colCount).map { col =>
          val hasSymbolNeighbor =
            getNeighbors(row, col, rowCount, colCount).exists { case (r, c) =>
              isSymbol(engineMatrix(r)(c))
            }

          if (hasSymbolNeighbor) '1' else '0';
        }
      )

    val engineVector: List[Char] = engineMatrix.map(_.mkString).mkString(".").toList
    val maskVector: List[Boolean] = symbolNeighborMask.map(_.mkString).mkString("0").toList.map(_ == '1')

    val res = getNumbers(engineVector, None, maskVector, false, List.empty)
    res
  }

}
