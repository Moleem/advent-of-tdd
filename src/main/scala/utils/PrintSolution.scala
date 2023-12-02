package utils

import java.io.{InputStream, PrintStream}

object PrintSolution {
  def apply[T, R](
                           inputFileName: String,
                           contentParser: ContentParser[T],
                           problemSolver: ProblemSolver[T, R],
                           fileReader: FileReader = MyLittleFileReader,
                           printStream: PrintStream = System.out
                         ): Unit = {
    val content = fileReader.readFile(inputFileName)
    val parsedContent = contentParser.parse(content)
    val solution = problemSolver.solve(parsedContent)
    printStream.println(solution)
  }

}
