package utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, PrintStream}


class PrintSolutionSpec extends AnyFlatSpec with Matchers {

  behavior of "PrintSolution"

  it should "print the solution for a test input file" in {
    val inputFileName = "whatever.txt"

    val contentParser: ContentParser[Seq[String]] =
      (content: String) => content.split("\n")

    val problemSolver: ProblemSolver[Seq[String], Int] =
      (input: Seq[String]) => input.map(_.length).sum

    val fileReader = new FileReader {
      override def readFile(fileName: String): String =
        """line1
          |line2""".stripMargin
    }


    val output = new ByteArrayOutputStream
    val printStream = new PrintStream(output)

    PrintSolution(
      inputFileName,
      contentParser,
      problemSolver,
      fileReader,
      printStream
    )

    output.toString.trim shouldBe "10" // len("line1") + len("line2")
  }

}
