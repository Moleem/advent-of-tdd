package day14.solvers

import utils.ProblemSolver

import scala.annotation.tailrec
import scala.collection.mutable

class AdvancedPlatformTilter(cycleDirections: List[Char], cycleCount: Int) extends ProblemSolver[String, String] {

  private val cache = new mutable.HashMap[String, Int]()

  override def solve(input: String): String =
    getCycleOutput(0, input)

  @tailrec
  private def getCycleOutput(cycleId: Int, input: String): String = {
    if (cycleId >= cycleCount) input
    else {
      if (cache.contains(input)) {
        val loopStart = cache(input)
        val loopLength = cycleId-loopStart
        val targetModulo = (cycleCount - loopStart) % loopLength + loopStart

        cache.find { case (key, id) => id == targetModulo }.get._1
      } else {
        cache.put(input, cycleId)

        getCycleOutput(cycleId+1, input.tiltInCycle(cycleDirections))
      }

    }
  }


  implicit class TurnableString(s: String) {

    def turnClockwise: String =
      s.split("\n").map(_.toList).toList.transpose.map(_.reverse.mkString).mkString("\n")

    def tiltUp: String = {
      val originalColumns = s.split("\n").map(_.toList).toList.transpose
      val tiltedColumns = originalColumns.map { column =>
        column.mkString.split("#", -1).map { partBetweenFixStones =>
          val (spaces, rollingStones) = partBetweenFixStones.partition(_ == 'O')
          spaces.mkString + rollingStones.mkString
        }.mkString("#")
      }

      tiltedColumns.transpose.map(_.mkString).mkString("\n")
    }

    def tiltNorth: String =
      s.tiltUp

    def tiltWest: String =
      s.turnClockwise
       .tiltUp
       .turnClockwise
       .turnClockwise
       .turnClockwise

    def tiltSouth: String =
      s.turnClockwise
       .turnClockwise
       .tiltUp
       .turnClockwise
       .turnClockwise

    def tiltEast: String =
      s.turnClockwise
       .turnClockwise
       .turnClockwise
       .tiltUp
       .turnClockwise

    def tiltInCycle(cycleDirections: List[Char]): String =
      cycleDirections.foldLeft(s) { case (sAfter, dir) =>
        dir match {
          case 'N' => sAfter.tiltNorth
          case 'W' => sAfter.tiltWest
          case 'S' => sAfter.tiltSouth
          case 'E' => sAfter.tiltEast
        }
      }
  }

}
