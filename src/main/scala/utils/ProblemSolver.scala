package utils

trait ProblemSolver[T, R] {
  def solve(input: T): R
}
