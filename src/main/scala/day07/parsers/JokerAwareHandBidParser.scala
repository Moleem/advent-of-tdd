package day07.parsers

import day07.model.{JokerAwareHand, JokerAwareHandBid}
import utils.ContentParser

object JokerAwareHandBidParser extends ContentParser[List[JokerAwareHandBid]] {
  override def parse(content: String): List[JokerAwareHandBid] =
    content.split("\n").toList
      .map{line =>
        val parts = line.split(" ")
        val hand = JokerAwareHand(parts(0))
        val bid = parts(1).toInt
        JokerAwareHandBid(hand, bid)
      }
}
