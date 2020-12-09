package day09

import utils.BaseApp

case class XMASCrypt(preambleSize: Int) {
  var items: Vector[Long] = Vector()
  var sortedItems: Vector[Long] = Vector()
  private var currentSize = 0

  def add(item: Long): Unit = {
    var newItems = items appended item
    currentSize += 1
    if (currentSize > preambleSize) {
      newItems = newItems.tail
      currentSize -= 1
    }
    items = newItems
    sortedItems = newItems.sorted
  }

  def canAdd(item: Long): Boolean = {
    currentSize < preambleSize || hasItemSum(item)
  }

  private def hasItemSum(item: Long): Boolean = {
    def search(from: Int, to: Int): Boolean = {
      if (from >= to) false
      else {
        val sum = sortedItems(from) + sortedItems(to)
        if (sum == item) true
        else if (sum < item) search(from + 1, to)
        else search(from, to - 1)
      }
    }

    search(0, currentSize - 1)
  }

  def findSumSet(item: Long, items: Vector[Long]): Vector[Long] = {
    def search(from: Int, to: Int, currentSum: Long): (Int, Int) = {
      if (currentSum == item) (from, to)
      else {
        if (currentSum < item) {
          val newTo = to + 1
          val newItem = items(newTo)
          val newSum = currentSum + newItem
          search(from, newTo, newSum)
        } else {
          val newFrom = from + 1
          val oldFrom = items(from)
          val newSum = currentSum - oldFrom
          search(newFrom, to, newSum)
        }
      }
    }

    val (from, to) = search(0, 0, items(0))
    items.slice(from, to + 1)
  }
}

object App extends BaseApp[Long, Long] {
  lazy val numbers = getLines.map(_.toLong).toList

  override def partI(): Long = {
    val state = XMASCrypt(25)

    def search(numbers: Seq[Long], crypt: XMASCrypt): Long = {
      val num = numbers.head
      val others = numbers.tail
      if (!crypt.canAdd(num)) num
      else {
        crypt.add(num)
        search(others, crypt)
      }
    }

    search(numbers, state)
  }

  override def partII(): Long = {
    val state = XMASCrypt(25)
    val vec = state.findSumSet(50047984, numbers.toVector)
    vec.min + vec.max
  }
}
