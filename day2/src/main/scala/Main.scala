import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val data = Source.fromResource("data.txt").getLines.map(parseLine).toList
    val validPasswordCount = data.count(l => checkPolicy(l.policy, l.password))
    println(validPasswordCount)
    val validPasswordCountPart2 = data.count(l => checkPolicyPart2(l.policy, l.password))
    println(validPasswordCountPart2)

    assert(checkPolicyPart2(Policy(1, 3, 'a'), "abcde"))
    assert(!checkPolicyPart2(Policy(1, 3, 'b'), "cdefg"))
    assert(!checkPolicyPart2(Policy(2, 9, 'c'), "ccccccccc"))
  }

  def parseLine(line: String): Line = {
    line.split(":") match {
      case Array(policy, password) => {
        Line(parsePolicy(policy), password.strip)
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

  def checkPolicyPart2(policy: Policy, password: String): Boolean = {
    val firstCharMatches = password.charAt(policy.from - 1) == policy.char
    val secondCharMatches = password.charAt(policy.to - 1) == policy.char

    firstCharMatches ^ secondCharMatches
  }
}

case class Line(policy: Policy, password: String)
case class Policy(from: Int, to: Int, char: Char)
