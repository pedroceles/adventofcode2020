package day06

import utils.BaseApp


case class GroupQuestions(answers: String) {
  private val charRegex = "[a-z]".r
  lazy val charSet: Set[Char] = answers.filter(ch => charRegex.matches(ch.toString)).toSet
  lazy val personCharIntersection = {
    val peopleAnswersSet = answers.split("\n").map(_.toSet)
    peopleAnswersSet.tail.foldLeft(peopleAnswersSet.head)(_.intersect(_))
  }
  lazy val uniqueCharCount = charSet.size
}

object App extends BaseApp[Int, Int] {
  lazy val questions = readlines.split("\n\n").map(GroupQuestions)

  override def partI(): Int = {
    questions.map(_.uniqueCharCount).sum
  }
  override def partII(): Int = {
    questions.map(_.personCharIntersection.size).sum
  }
}
