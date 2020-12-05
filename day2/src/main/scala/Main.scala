import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val data = Source.fromResource("data.txt").getLines.map(parseLine).toList
    val validPasswordCount = data.filter(l => checkPolicy(l.policy, l.password)).size
    println(validPasswordCount)
  }

  def parseLine(line: String): Line = {
    line.split(":") match {
      case Array(policy, password) => {
        Line(parsePolicy(policy), password)
      }
    }
  }

  def parsePolicy(policy: String): Policy = {
    val parts = policy.split(" ")
    val range = parts(0).split("-")

    Policy(range(0).toInt, range(1).toInt, parts(1).charAt(0))
  }

  def checkPolicy(policy: Policy, password: String): Boolean = {
    val charCount = password.count(char => char == policy.char)
    charCount >= policy.from && charCount <= policy.to
  }
}

case class Line(policy: Policy, password: String)
case class Policy(from: Int, to: Int, char: Char)
