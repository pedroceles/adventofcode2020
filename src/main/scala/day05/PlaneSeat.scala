package day05

import utils.BaseApp

case class PlaneSeat(row: Int, column: Int) {
  def id: Int = row * 8 + column
}

object PlaneSeat {
  def apply(string: String): PlaneSeat = {
    val rowCharMap = Map(
      'F' -> false,
      'B' -> true,
    )
    val colCharMap = Map(
      'R' -> true,
      'L' -> false,
    )
    val (row, column) = string.splitAt(7)
    val rowBits = row.map(rowCharMap)
    val colBits = column.map(colCharMap)
    PlaneSeat(bitsToInt(rowBits), bitsToInt(colBits))
  }

  def bitsToInt(bits: Seq[Boolean]): Int = {
    bits.reverse.zipWithIndex.foldLeft(0)((n, el) => {
      val (bit, index) = el
      if (bit) Math.pow(2, index).toInt + n
      else n
    })
  }
}

object App extends BaseApp[Int, Int] {
  lazy val seats = getLines.map(PlaneSeat(_))

  override def partI(): Int = {
    seats.map(_.id).max
  }

  override def partII(): Int = {
    val seatSet = seats.map(_.id).toSet
    val missingSet = (0 until 128).foreach(row => {
      (0 until 8).foreach(col => {
        val seat = PlaneSeat(row, col)
        val id = seat.id
        val hasId = seatSet contains id
        val hasPrev = seatSet contains (id - 1)
        val hasNext = seatSet contains (id + 1)
        if (!hasId && hasNext && hasPrev) println(seat, seat.id)
      })
    })
    1
  }
}
