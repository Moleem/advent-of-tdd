package day07.parsers

import day07.model.{Hand, HandBid}
import utils.ContentParser

object HandBidParser extends ContentParser[List[HandBid]] {
  override def parse(content: String): List[HandBid] =
    content.split("\n").toList
      .map{line =>
        val parts = line.split(" ")
        val hand = Hand(parts(0))
        val bid = parts(1).toInt
        HandBid(hand, bid)
      }
}
