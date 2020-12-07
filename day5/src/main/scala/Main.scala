import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.txt").getLines().toList
    val maxId = input.map(l => l.splitAt(7)).map{case (rowId, seatId) => calculateRowNumber(rowId.toList) * 8 + calulateSeatNumber(seatId.toList)}.max

    println(maxId)
  }

  def calculateRowNumber(rowId: List[Char]) = calculateItemNumber(rowId, 'F', 'B')
  def calulateSeatNumber(seatId: List[Char]) = calculateItemNumber(seatId, 'L', 'R')

  def calculateItemNumber(itemId: List[Char], firstHalfChar: Char, secondHalfChar: Char) = {
    def calculateRowNumberInRange(rowRange: Range, rowId: List[Char]): Int = {
      println(rowRange)
      rowId match {
        case Nil => rowRange.end
        case x :: xs => if (x == firstHalfChar) calculateRowNumberInRange(rowRange.firstHalf(), xs) else calculateRowNumberInRange(rowRange.secondHalf(), xs)
      }
    }
    calculateRowNumberInRange(Range(0, (math.pow(2, itemId.size) - 1).toInt), itemId)
  }
}
case class Range(start: Int, end: Int) {
  def middleElement(): Int = {
    this.start + (end - start) / 2
  }

  def firstHalf() = {
    Range(start, middleElement())
  }

  def secondHalf() = {
    Range(middleElement(), end)
  }
}


