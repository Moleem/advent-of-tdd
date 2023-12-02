package day01.parsers

import day01.model.Calibration
import utils.ContentParser

object NaiveCalibrationParser extends ContentParser[Seq[Calibration]] {
  private def findFirstDigit(line: String): Option[Int] =
    line.find(Character.isDigit).map(Character.getNumericValue)

  private def findLastDigit(line: String): Option[Int] =
    findFirstDigit(line.reverse)

  override def parse(content: String): Seq[Calibration] =
    content
      .split("\n")
      .map(line =>
        (findFirstDigit(line), findLastDigit(line)) match {
          case (Some(firstDigit), Some(lastDigit)) => Calibration(firstDigit, lastDigit)
          case _ => throw new IllegalStateException
        }
      )
}
