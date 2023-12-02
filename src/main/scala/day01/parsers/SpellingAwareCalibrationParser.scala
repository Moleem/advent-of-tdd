package day01.parsers

import day01.model.Calibration
import utils.ContentParser

import scala.annotation.tailrec

object SpellingAwareCalibrationParser extends ContentParser[Seq[Calibration]] {

  private val digitsAndNumericValues: Map[String, Int] = Map(
    "0" -> 0,
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  private def getNumericValue(chunk: String): Option[Int] = digitsAndNumericValues.get(chunk)

  @tailrec
  private def findFirstDigit(line: String): Option[Int] =
    if (line == "") None
    else digitsAndNumericValues.keys.find(line.startsWith) match {
      case Some(matchingPrefix) => getNumericValue(matchingPrefix)
      case None => findFirstDigit(line.tail)
    }

  @tailrec
  private def findLastDigit(line: String): Option[Int] =
    if (line == "") None
    else digitsAndNumericValues.keys.find(line.endsWith) match {
      case Some(matchingSuffix) => getNumericValue(matchingSuffix)
      case None => findLastDigit(line.init)
    }

  override def parse(content: String): Seq[Calibration] =
    content
      .split("\n")
      .map(line =>
        (findFirstDigit(line), findLastDigit(line)) match {
          case (Some(firstDigit), Some(lastDigit)) => Calibration(firstDigit, lastDigit)
          case _ => throw new IllegalStateException
        }
      ).toSeq
}
