package day04.solvers

import day04.model.ScratchCardRecord
import utils.ProblemSolver

import scala.annotation.tailrec

object CountCards extends ProblemSolver[List[ScratchCardRecord], Int] {

  @tailrec
  private def processCards(pile: List[Int], cardScores: Map[Int, Int], cardProcessCounts: Map[Int, Int]): Int =
    pile match {
      case Nil =>
        cardProcessCounts.values.sum
      case next :: rest =>
        val winCountForNext = cardScores(next)
        val cardsToModify =
          if (winCountForNext > 0) (next + 1 to next + winCountForNext).toList
          else List.empty

        val newCounts =
          cardProcessCounts.map { case (key, value) =>
            if (cardsToModify.contains(key)) key -> (value + cardProcessCounts(next))
            else key -> value
          }

        processCards(rest, cardScores, newCounts)
    }


  override def solve(input: List[ScratchCardRecord]): Int = {
    val cardScores: Map[Int, Int] =
      input
        .map(card => card.lotteryNumbers.intersect(card.ownNumbers))
        .map(_.size)
        .zipWithIndex
        .map { case (count, index) => (index+1) -> count}
        .toMap

    val cardProcessCounts: Map[Int, Int] =
      cardScores
        .keys
        .map { _ -> 1 }
        .toMap

    processCards(cardScores.keys.toList.sorted, cardScores, cardProcessCounts)
  }
}