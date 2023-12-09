package utils

trait ProblemSolver[T, R] {
  def solve(input: T): R
  def andThen[U](other: ProblemSolver[R, U]): ProblemSolver[T, U] = ???
}
