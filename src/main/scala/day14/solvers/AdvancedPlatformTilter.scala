package day14.solvers

import day14.model.Stone
import utils.ProblemSolver

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

        val cycleOutput = runCycle(input)
        getCycleOutput(cycleId+1, cycleOutput)
      }

    }
  }

  private def runCycle(input: String): String = {
    cycleDirections.foldLeft(input) { case (s, dir) =>
      dir match {
        case 'N' => tiltNorth(s)
        case 'W' => tiltWest(s)
        case 'S' => tiltSouth(s)
        case 'E' => tiltEast(s)
      }
    }
  }


  private def tiltUp(input: String): String = {
    val originalColumns = input.split("\n").map(_.toList).toList.transpose
    val tiltedColumns = originalColumns.map { column =>
      column.mkString.split("#", -1).map { partBetweenFixStones =>
        val (spaces, rollingStones) = partBetweenFixStones.partition(_ == 'O')
        spaces.mkString + rollingStones.mkString
      }.mkString("#")
    }

    tiltedColumns.transpose.map(_.mkString).mkString("\n")
  }

  private def turnClockwise(input: String): String =
    input.split("\n").map(_.toList).toList.transpose.map(_.reverse.mkString).mkString("\n")

  private def tiltNorth(input: String): String =
    tiltUp(input)

  private def tiltWest(input: String): String =
    turnClockwise(turnClockwise(turnClockwise(tiltUp(turnClockwise(input)))))

  private def tiltSouth(input: String): String =
    turnClockwise(turnClockwise(tiltUp(turnClockwise(turnClockwise(input)))))

  private def tiltEast(input: String): String =
    turnClockwise(tiltUp(turnClockwise(turnClockwise(turnClockwise(input)))))

}
