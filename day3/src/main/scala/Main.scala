import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.txt").getLines().toList
    println(ride(0, 0, input))
  }

  def isTree(pos: Int, line: String) = line.charAt(pos % line.length) == '#'

  def ride(position: Int, treesEncountered: Int, m: List[String]): Int = {
    m match {
      case Nil => treesEncountered
      case x :: xs => if (isTree(position, x)) ride(nextPosition(position, x.length), treesEncountered + 1, xs) else ride(nextPosition(position, x.length), treesEncountered, xs)
    }
  }

  def nextPosition(position: Int, lineLength: Int) = (position + 3) % lineLength
}
