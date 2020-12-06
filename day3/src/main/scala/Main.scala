import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.txt").getLines().toList

    // Part 1
    println(ride(0, 0, 3, 1, 0, input))

    // Part 2
    val result = ride(0, 0, 1, 1, 0, input).toLong *
    ride(0, 0, 3, 1, 0, input) *
    ride(0, 0, 5, 1, 0, input) *
    ride(0, 0, 7, 1, 0, input) *
    ride(0, 0, 1, 2, 0, input)
    println(result)
  }

  def isTree(posX: Int, posY: Int, m: List[String]) = {
    val line = m(posY)
    line.charAt(posX % line.length) == '#'
  }

  def ride(posX: Int, posY: Int, deltaX: Int, deltaY: Int, treesEncountered: Int, m: List[String]): Int = {
    if (posY >= m.size) treesEncountered else {
      if (isTree(posX, posY, m)) {
        ride(posX + deltaX, posY + deltaY, deltaX, deltaY, treesEncountered + 1, m)
      } else {
        ride(posX + deltaX, posY + deltaY, deltaX, deltaY, treesEncountered, m)
      }
    }
  }

}
