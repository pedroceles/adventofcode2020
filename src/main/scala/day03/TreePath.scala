package day03

import scala.io.Source

case class MapPosition(baseMap: Array[String], x: Int = 0, y: Int = 0) {
  lazy val baseMapHeight = baseMap.length
  lazy val baseMapLength = baseMap(0).length
  lazy val getChar = baseMap(y)(x)
  lazy val hasTree = getChar == '#'
  lazy val isOnBottom = y == baseMapHeight - 1
//  println(x, y, getChar)

  def move(dx: Int, dy: Int): MapPosition = {
//    println(baseMap.mkString("\n"))
//    println(getChar.toString)
    val newX = (x + dx) % baseMapLength
    val newY = y + dy
    MapPosition(baseMap, newX, newY)
  }
}

case class TravelInfo(position: MapPosition, nTrees: Int = 0)

object TreePath {
  def readFile = {
    val stream = getClass.getResourceAsStream("day03.txt")
    val source = Source.fromInputStream(stream)
    val lines = source.getLines.toArray
    source.close
    lines
  }
  def getMap = MapPosition(readFile)
  def printReturn[A](a: A): A = {
    println(a)
    a
  }

  def travel(dx: Int, dy: Int): TravelInfo = {
    def doTravel(travelInfo: TravelInfo): TravelInfo = {
      val position = travelInfo.position
      val newTreeCount = position.hasTree match {
        case true => 1 + travelInfo.nTrees
        case false => travelInfo.nTrees
      }
      if (position.isOnBottom) TravelInfo(position, newTreeCount)
      else {
        val newPos = position.move(dx, dy)
        val newInfo = TravelInfo(newPos, newTreeCount)
        doTravel(newInfo)
      }
    }
    doTravel(TravelInfo(getMap))
  }
  def multiTravel: Long = {
    val steps = List[(Int, Int)](
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2),
    )
    val totalTrees: List[Long] = steps.map{
      case (dx, dy) => printReturn({println("-------------------"); travel(dx ,dy).nTrees})
    }
    println(totalTrees, totalTrees.product)
    totalTrees.product
  }

  def main(args: Array[String]): Unit = {
//    println("PART I", travel(1, 1).nTrees)
//    println("PART I", travel(3, 1).nTrees)
//    println("PART I", travel(5, 1).nTrees)
//    println("PART I", travel(7, 1).nTrees)
//    println("PART I", travel(1, 2).nTrees)
    println("PART II", multiTravel)
  }
}
