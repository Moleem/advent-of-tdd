package day12.solvers

import utils.ProblemSolver

import scala.collection.mutable

object SpringErrorMatcher extends ProblemSolver[List[(String, List[Int])], Long] {

  private val cache = new mutable.HashMap[(String, List[Int]), Long]()

  override def solve(input: List[(String, List[Int])]): Long =
    input.map { case (pattern, nums) => getArrangementCount(pattern, nums) }.sum

  private def getArrangementCount(pattern: String, groups: List[Int]): Long = {
    if (cache.contains((pattern, groups))) cache((pattern, groups))
    else {
      if (groups.isEmpty && !pattern.contains("#")) 1L
      else if (groups.isEmpty && pattern.contains("#")) 0L
      else {
        var res = 0L

        val firstGroup = groups.head
        val remainingGroups = groups.tail
        var shouldRun = true
        val expectedGroupEnd = pattern.length - remainingGroups.sum - remainingGroups.length - firstGroup + 1

        (0 until expectedGroupEnd).foreach { i =>
          if (pattern.substring(0, i).contains('#')) {
            shouldRun = false
          } else {
            if (shouldRun) {
              val possibleGroupEnd = i + firstGroup
              val fitsThePatternSize = possibleGroupEnd <= pattern.length
              val hasNoGood = !pattern.substring(i, possibleGroupEnd).contains('.')
              val isNotFollowedByBad = pattern.length < possibleGroupEnd + 1 || pattern.toList(possibleGroupEnd) != '#'

              if (fitsThePatternSize && hasNoGood && isNotFollowedByBad) {
                if (pattern.length >= possibleGroupEnd + 1)
                  res = res + getArrangementCount(pattern.substring(possibleGroupEnd + 1), remainingGroups)
                else
                 res = res + getArrangementCount("", remainingGroups)
              }
            }
          }
        }

        cache.put((pattern, groups), res)
        res
      }
    }
  }

}