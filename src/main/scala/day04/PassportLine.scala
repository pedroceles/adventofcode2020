package day04

import utils.BaseApp

import scala.util.matching.Regex

abstract class Validator() {
  def isValid(str: String): Boolean = false
}

class RegexValidator(protected val re: Regex) extends Validator {
  override def isValid(str: String): Boolean = re.matches(str)

  def getMatch(str: String) = re.findFirstMatchIn(str).get
}

class IntBoundValidator(from: Int, to: Int) extends RegexValidator("\\d+".r) {
  override def isValid(str: String): Boolean = {
    super.isValid(str) && str.toInt >= from && str.toInt <= to
  }
}

class HexColorValidator extends RegexValidator("#[a-f0-9]+".r)

class EyeColorValidator extends Validator {
  val acceptedColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  override def isValid(str: String): Boolean = acceptedColors.contains(str)
}

class PassportIDValidator extends RegexValidator("\\d{9}".r)

class HeightValidator extends RegexValidator("(\\d+)(in|cm)".r) {
  override def isValid(str: String): Boolean = {
    if (!super.isValid(str)) false
    else {
      str match {
        case re(h, m) => {
          if (m == "cm") new IntBoundValidator(150, 193).isValid(h)
          else new IntBoundValidator(59, 79).isValid(h)
        }
      }
    }
  }
}

case class Passport(fields: Map[String, String]) {
  val validatorsMap = Map(
    "byr" -> new IntBoundValidator(1920, 2002),
    "iyr" -> new IntBoundValidator(2010, 2020),
    "eyr" -> new IntBoundValidator(2020, 2030),
    "hgt" -> new HeightValidator(),
    "hcl" -> new HexColorValidator(),
    "ecl" -> new EyeColorValidator(),
    "pid" -> new PassportIDValidator(),
  )
  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def hasAllRequiredFields: Boolean = requiredFields subsetOf fields.keySet
  def isValid: Boolean = {
    requiredFields.forall(field => {
      val validator = validatorsMap(field)
      val value = fields.getOrElse(field, "")
      val isValid = validator.isValid(value)
      if(!isValid) {
      }
      isValid
    })
  }
}

object Passport {
  def apply(string: String): Passport = {
    val fields = string
      .replaceAll("\n", " ")
      .split(" ")
      .filter(_.length > 0)
      .map(kv => {
        val split = kv.split(":")
        split(0) -> split(1)
      })
    val map = Map(fields: _*)
    Passport(map)
  }
}

object App extends BaseApp[Int, Int] {
  override def getLines: Iterable[String] = readlines.split("\n\n")
  lazy val passaports = getLines.map(Passport(_))

  override def partI(): Int = {
    passaports.filter(_.hasAllRequiredFields).toList.length
  }

  override def partII(): Int = {
    passaports.filter(_.isValid).toList.length
  }
}
