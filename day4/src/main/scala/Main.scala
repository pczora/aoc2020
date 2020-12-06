import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val rawData = Source.fromResource("input.txt").getLines().toList
    val cleanedData = mergePassportString(rawData)
    val passportData = cleanedData
      .map(l => {
        val attributes = l.split(" ").toList
        attributes.map(parseAttributeTuple)
      })
      .map(attributeListToMap)

    val validPasswordCount = passportData
      .filter(isValidPassport)
      .size
    println(validPasswordCount)

    // Part 2
    val validPasswordCount2 = passportData.filter(isValidPassportPart2).size
    println(validPasswordCount2)
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

  def isValidYear(year: String, min: Int, max: Int): Boolean = {
    val intYear = year.toInt
    min <= intYear && intYear <= max
  }

  def isValidBirthYear(byr: String) = isValidYear(byr, 1920, 2002)

  def isValidIssueYear(iyr: String) = isValidYear(iyr, 2010, 2020)

  def isValidExpirationYear(eyr: String) = isValidYear(eyr, 2020, 2030)

  def isValidHeight(hgt: String): Boolean = {

    def isValidHeightInCm(height: String) = {
      if (height.contains("cm")) {
        val heightInCm = height.split("cm")(0).toInt
        150 <= heightInCm && heightInCm <= 193
      } else false
    }

    def isValidHeightInIn(height: String) = {
      if (height.contains("in")) {
        val heightInInches = height.split("in")(0).toInt
        59 <= heightInInches && heightInInches <= 76
      } else false
    }

    isValidHeightInCm(hgt) || isValidHeightInIn(hgt)
  }

  def isValidHairColor(hcl: String) = {
    if (hcl.startsWith("#")) {
      val value = hcl.split("#")(1)
      value.size == 6 && value.forall(c => "abcdef0123456789".contains(c))
    } else false
  }

  def isValidEyeColor(ecl: String) = {
    val validColors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    validColors.contains(ecl)
  }

  def isValidPassportId(pid: String) = {
    pid.size == 9 && pid.forall(c => "0123456789".contains(c))
  }

  def isValidPassportPart2(passport: Map[String, String]): Boolean = {
    isValidPassport(passport) && isValidBirthYear(
      passport("byr")
    ) && isValidIssueYear(passport("iyr")) && isValidExpirationYear(
      passport("eyr")
    ) && isValidHeight(passport("hgt")) && isValidHairColor(
      passport("hcl")
    ) && isValidEyeColor(passport("ecl")) && isValidPassportId(passport("pid"))
  }
}
