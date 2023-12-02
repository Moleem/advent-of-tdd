package day02.task1

import utils.MyLittleFileReader

object Runner extends App {
  val content = MyLittleFileReader.readFile("/input02-1.txt")
  val solution = new Solution(content)
  println(solution.sumPossible)
}
