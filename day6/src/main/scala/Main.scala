import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.txt").getLines.toList
    val groups = groupList(input)
    val part1 = groups.map(group => group.foldLeft("")((z, l) => (z + l).distinct)).map(_.length).sum
    println(part1)
    
    val part2 = groups.map {
      case Nil => Nil
      case x :: xs =>
        commonChars(x, xs)
    }.map(_.size).sum
    println(part2)
  }

  def groupList(input: List[String]): List[List[String]] = {
    @tailrec
    def groupListAcc(acc: List[List[String]], input: List[String]): List[List[String]] = {
      if (input.isEmpty) acc else {
       val s = input.span(l => !l.isBlank)
       s match {
         case (group, groups) =>
           if (group.nonEmpty) groupListAcc(group :: acc, groups) else groupListAcc(acc, groups.drop(1))
       }
      }
    }
    groupListAcc(List(), input)
  }


  def commonChars(s: String, xs: List[String]): List[Char] = {
    @tailrec
    def commonCharsAcc(acc: List[Char], s: List[Char], xs: List[String]): List[Char] = {
      s match {
        case Nil => acc
        case y :: ys => if (xs.forall(x => x.contains(y))) commonCharsAcc(y :: acc, ys, xs) else commonCharsAcc(acc, ys, xs)
      }
    }
    commonCharsAcc(List(), s.toList, xs)
  }

}
