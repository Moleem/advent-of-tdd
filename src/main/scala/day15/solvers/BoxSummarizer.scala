package day15.solvers

import day15.model.Lens
import utils.ProblemSolver

object BoxSummarizer extends ProblemSolver[Map[Int, List[Lens]], Int] {

  override def solve(input: Map[Int, List[Lens]]): Int =
    input.flatMap { case (boxId, lenses) =>
      lenses.zipWithIndex.map { case (lens, lensId) =>
        (boxId + 1) * (lensId + 1) * lens.focalLength
      }
    }.sum

}
