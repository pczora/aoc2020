import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.txt").getLines.toList
    println(groupNumbers(input).sum)
  }

  def groupNumbers(input: List[String]) = {
    def groupNumbersAcc(acc: List[Int], input: List[String]): List[Int] = {
      input match {
        case Nil => acc
        case x :: xs => {
          val s = input.span(l => !l.isBlank)
          s match {
            case (group, groups) => {
              if (!group.isEmpty) {
                val groupUniqueChars = uniqueChars(group.foldLeft("")((z, l) => z + l)).size
                groupNumbersAcc(groupUniqueChars :: acc, groups)
              } else {
                groupNumbersAcc(acc, groups.drop(1))
              }
            }
          }
        }
      }
    }
    groupNumbersAcc(List(), input)
  }
  def uniqueChars(input: String) = {
    def uniqueCharsAcc(acc: List[Char], input: List[Char]): List[Char] = {
      input match {
        case Nil => acc
        case x :: xs => if (acc.contains(x)) uniqueCharsAcc(acc, xs) else uniqueCharsAcc(acc ::: List(x), xs)
      }
    }
    uniqueCharsAcc(List(), input.toList)
  }
}
