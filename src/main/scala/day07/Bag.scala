package day07

import utils.BaseApp

case class Bag(name: String, contents: Seq[(Int, String)] = Nil) {
}

object Bag {
  val lineSample = "vibrant lavender bags contain 1 shiny coral bag, 4 dotted purple bags."
  private val emptyBagRe = "(\\w+ \\w+) bags contain no .+\\.".r
  private val re = "(\\w+ \\w+) bags contain (.+)\\.".r

  def fromRawString(string: String): Bag = {
    string match {
      case emptyBagRe(bagName) => {
        Bag(bagName)
      }
      case re(bagName, otherBagsStr) => {
        val otherBags = otherBagsStr.split(", ").map(
          bagNameWithNumber => {
            val spaceSplits = bagNameWithNumber.split(" ")
            val contentName = spaceSplits.slice(1, 3).mkString(" ")
            val contentAmount = spaceSplits.head.toInt
            (contentAmount, contentName)
          }
        )
        Bag(bagName, otherBags)
      }
    }
  }
}

object App extends BaseApp[Int, Int] {
  lazy val bags = getLines.map(Bag.fromRawString)
  lazy val insideOutMap: Map[String, Set[String]] = {
    val initialMap: Map[String, Set[String]] = Map().withDefault(_ => Set())
    bags.foldLeft(initialMap)((map, bag) => {
      bag.contents.foldLeft(map)((bagMap, content) => {
        val set = bagMap(content._2)
        bagMap.updated(content._2, set + bag.name)
      })
    })
  }
  lazy val ownershipMap: Map[String, Set[(Int, String)]] = {
    val initialMap: Map[String, Set[(Int, String)]] = Map().withDefault(_ => Set())
    bags.foldLeft(initialMap)((map, bag) => {
      val newSet = map(bag.name) ++ bag.contents
      map updated (bag.name, newSet)
    })
  }

  def countOwnership(bags: Set[String], visited: Set[String], acc: Int = 0): (Int, Set[String]) = {
    val initialState = (acc, visited)
    if (bags.isEmpty) initialState
    else {
      bags.foldLeft(initialState)((state, bag) => {
        val (currentCount, currentVisited) = state
        val alreadyVisited = currentVisited.contains(bag)
        if (alreadyVisited) state
        else {
          val toVisit = insideOutMap(bag)
          val newVisited = currentVisited + bag
          val newAcc = currentCount + 1
          countOwnership(toVisit, newVisited, newAcc)
        }
      })
    }
  }

  def countBagsWithin(bags: Set[(Int, String)]): Int = {
    val insideCount = bags.foldLeft(0)((sum, bagInfo) => {
      val (bagCount, bagName) = bagInfo
      val insideBags = ownershipMap(bagName)
      val newSum = sum + bagCount * countBagsWithin(insideBags)
      newSum
    })
    insideCount + 1
  }

  override def partI(): Int = {
    countOwnership(insideOutMap("shiny gold"), Set("shiny gold"))._1
  }

  def partII(): Int = {
    countBagsWithin(ownershipMap("shiny gold")) - 1
  }
}