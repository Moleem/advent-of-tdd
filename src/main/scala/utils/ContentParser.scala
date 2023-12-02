package utils

trait ContentParser[T] {
  def parse(content: String): T
}
