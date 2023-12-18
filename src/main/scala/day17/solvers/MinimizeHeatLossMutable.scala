package day17.solvers

import utils.ProblemSolver

import scala.annotation.tailrec
import scala.collection.mutable


object MinimizeHeatLossMutable extends ProblemSolver[String, Int] {

  private case class Coordinate(row: Int, col: Int) {
    def possibleNeighbors: Map[Direction, Coordinate] = Map(
      Up    -> Coordinate(row-1, col),
      Down  -> Coordinate(row+1, col),
      Right -> Coordinate(row, col+1),
      Left  -> Coordinate(row, col-1),
    )

    def getNeighborAt(direction: Direction): Coordinate =
      possibleNeighbors(direction)
  }

  private sealed trait Direction {
    val oppositeDirection: Direction
  }
  private case object Up extends Direction {
    override val oppositeDirection: Direction = Down
  }
  private case object Down extends Direction {
    override val oppositeDirection: Direction = Up
  }
  private case object Right extends Direction {
    override val oppositeDirection: Direction = Left
  }
  private case object Left extends Direction {
    override val oppositeDirection: Direction = Right
  }

  override def solve(input: String): Int = {
    val numsMatrix = parseHeatLossMatrix(input)
    val start = Coordinate(0, 0)
    val end = Coordinate(numsMatrix.size - 1, numsMatrix.head.size - 1)
    val cityBlocks = parseCityBlocks(numsMatrix)

    implicit val heatLossDiscoveryStepOrdering = new Ordering[HeatLossDiscoveryStep] {
      override def compare(x: HeatLossDiscoveryStep, y: HeatLossDiscoveryStep): Int = Integer.compare(
        y.accumulatedHeatLoss,
        x.accumulatedHeatLoss
      )
    }
    val remainingSteps: mutable.PriorityQueue[HeatLossDiscoveryStep] = new mutable.PriorityQueue[HeatLossDiscoveryStep]
    remainingSteps.enqueue(HeatLossDiscoveryStep(start, 0, None, 0))

    val smallestKnownHeatLosses = new mutable.HashMap[CacheKey, Int]()

    @tailrec
    def getMinHeatLossMap: Map[CacheKey, Int] = {
      if (remainingSteps.isEmpty) {
        smallestKnownHeatLosses.toMap
      } else {
        val head = remainingSteps.dequeue()
        val key = CacheKey(
          head.coordinate,
          head.prevDirection.map(_.oppositeDirection).map(head.coordinate.getNeighborAt),
          head.straightStepsSoFar
        )

        if (smallestKnownHeatLosses.contains(key)) {
          getMinHeatLossMap
        } else {
          val possibleNeighbors = head.coordinate.possibleNeighbors
          val existingNeighbors = possibleNeighbors
            .filter { case (direction, coordinate) =>
              cityBlocks.contains(coordinate)
            }

          val forbiddenDirection = Option.when(head.straightStepsSoFar == 3)(head.prevDirection.get)

          val allowedNeighbors = existingNeighbors
            .filter { case (direction, coordinate) => !forbiddenDirection.contains(direction) && !head.prevDirection.contains(direction.oppositeDirection) }

          val reasonableNeighbors = allowedNeighbors.filter { case (direction, coordinate) =>
            !smallestKnownHeatLosses.get(
              CacheKey(coordinate, Some(head.coordinate), if (head.prevDirection.contains(direction)) head.straightStepsSoFar + 1 else 1)
            ).exists(_ < head.accumulatedHeatLoss + cityBlocks(coordinate))
          }

          val remainingNeighborsByHeatLoss = reasonableNeighbors.toList
            .sortBy { case (direction, coordinate) => cityBlocks(coordinate) }


          val newSteps = remainingNeighborsByHeatLoss.map { case (direction, coordinate) =>
            HeatLossDiscoveryStep(coordinate, head.accumulatedHeatLoss + cityBlocks(coordinate), Some(direction), if (head.prevDirection.contains(direction)) head.straightStepsSoFar + 1 else 1)
          }

          newSteps.foreach(elem => remainingSteps.enqueue(elem))

          smallestKnownHeatLosses.put(key, head.accumulatedHeatLoss)

          if (head.coordinate == Coordinate(140, 140))
            smallestKnownHeatLosses.toMap
          else
            getMinHeatLossMap
        }
      }
    }

    val heatLossMap = getMinHeatLossMap

    heatLossMap.toList.filter { case (key, value) => key.coordinate == end }.map { case (key, value) => value }.min
  }

  private def parseHeatLossMatrix(input: String): List[List[Int]] =
    input.split("\n").map(_.toList.map(Character.getNumericValue)).toList

  private def parseCityBlocks(numsMatrix: List[List[Int]]): Map[Coordinate, Int] =
    numsMatrix.zipWithIndex.flatMap { case (row, rowId) =>
      row.zipWithIndex.map { case (heatLoss, colId) =>
        Coordinate(rowId, colId) -> heatLoss
      }
    }.toMap

  private case class HeatLossDiscoveryStep(
                                            coordinate: Coordinate,
                                            accumulatedHeatLoss: Int,
                                            prevDirection: Option[Direction],
                                            straightStepsSoFar: Int
                                          )

  private case class CacheKey(
                               coordinate: Coordinate,
                               previousCoordinate: Option[Coordinate],
                               straightStepsSoFar: Int
                             )


}
