package utils

import scala.io.Source

trait FileReader {
  def readFile(fileName: String): String
}

object MyLittleFileReader extends FileReader {
  def readFile(fileName: String): String = {
    val source = Source.fromURL(getClass.getResource(fileName))
    val content = source.getLines.mkString("\n")

    source.close()

    content
  }

}
