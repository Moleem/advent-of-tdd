package day04.solvers

import day04.model.ScratchCardRecord
import utils.ProblemSolver

import scala.annotation.tailrec

object CountCards extends ProblemSolver[List[ScratchCardRecord], Int] {

  @tailrec
  private def processCards(pile: List[Int], cache: Map[Int, Int], processedCount: Int): Int =
    pile match {
      case Nil => processedCount
      case next :: rest =>
        val winCountForNext = cache(next)
        val cardsWon =
          if (winCountForNext > 0) (next+1 to next+winCountForNext).toList
          else List.empty
        processCards(rest ++ cardsWon, cache, processedCount+1)
    }

  override def solve(input: List[ScratchCardRecord]): Int = {
    val cardScores: Map[Int, Int] =
      input
        .map(card => card.lotteryNumbers.intersect(card.ownNumbers))
        .map(_.size)
        .zipWithIndex
        .map { case (count, index) => (index+1) -> count}
        .toMap

    processCards(cardScores.keys.toList.sorted, cardScores, 0)
  }
}