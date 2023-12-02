package day01.solvers

import day01.model.Calibration
import utils.ProblemSolver

object SumFirstAndLastDigits extends ProblemSolver[Seq[Calibration], Int] {

  override def solve(input: Seq[Calibration]): Int =
    input.map(calibration => calibration.firstDigit * 10 + calibration.lastDigit).sum

}
