package utils

import scala.io.Source
abstract class BaseApp[A, B] {
  def readlines: String = {
    val stream = getClass.getResourceAsStream("input.txt")
    Source.fromInputStream(stream).mkString("")
  }

  def getLines: Iterable[String] = readlines.split("\n")

  def partI(): A
  def partII(): B

  def main(args: Array[String]): Unit = {
    println("PART I", partI())
    println("PART II", partII())
  }
}
