import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val rawData = Source.fromResource("input.txt").getLines().toList
    val cleanedData = mergePassportString(rawData)
    val validPasswordCount = cleanedData
      .map(l => {
        val attributes = l.split(" ").toList
        attributes.map(parseAttributeTuple)
      })
      .map(attributeListToMap)
      .filter(isValidPassport)
      .size
    println(validPasswordCount)
  }

  def mergePassportString(rawData: List[String]): List[String] = {
    def mergePassportStringAcc(
        passportAcc: String,
        acc: List[String],
        rawData: List[String]
    ): List[String] = {
      rawData match {
        case Nil => acc ::: passportAcc :: Nil
        case x :: xs =>
          if (x.isBlank)
            mergePassportStringAcc("", acc ::: passportAcc :: Nil, xs)
          else mergePassportStringAcc((passportAcc + " " + x).trim, acc, xs)
      }
    }
    mergePassportStringAcc("", List(), rawData)
  }

  def parseAttributeTuple(attribute: String): (String, String) = {
    val a = attribute.split(":")
    (a(0).trim, a(1).trim)
  }

  def attributeListToMap(list: List[(String, String)]): Map[String, String] = {
    def attributeListToMapAcc(
        acc: Map[String, String],
        list: List[(String, String)]
    ): Map[String, String] = {
      list match {
        case Nil     => acc
        case x :: xs => attributeListToMapAcc(acc + (x._1 -> x._2), xs)
      }
    }
    attributeListToMapAcc(Map(), list)
  }

  def isValidPassport(passport: Map[String, String]): Boolean = {
    passport.contains("byr") && passport.contains("iyr") && passport.contains(
      "eyr"
    ) && passport.contains("hgt") &&
    passport.contains("hcl") && passport.contains("ecl") && passport.contains(
      "pid"
    )
  }
}
