package aoc2018

import scala.io.Source

object Day02 {

  val testIn = List(
    "abcdef",
    "bababc",
    "abbcde",
    "abcccd",
    "aabcdd",
    "abcdee",
    "ababab"
  )

  val testIn2 = List(
    "abcde",
    "fghij",
    "klmno",
    "pqrst",
    "fguij",
    "axcye",
    "wvxyz"
  )

  def main(args: Array[String]): Unit = {
    val input = parse("src/main/resources/aoc2018/day02.txt")
    println(input.size)

    val res1 = ex1(input)
    println(s"RES1=$res1")
    val res2 = ex2(input)
    println(s"RES2=$res2")

  }

  val TwoRegEx = "^(.)[]"

  def ex1(ids: List[String]) = {
    val two = ids.count(hasTwo)
    val three = ids.count(hasThree)
    two * three
  }

  def ex2(ids: List[String]): String = ids match {
    case head :: tail =>
      tail.find { str => diffBy1(head, str) } match {
        case Some(str) => (head zip str).filter { case (a, b) => a == b }.unzip._1.mkString
        case None => ex2(tail)
      }
    case Nil => ""


  }

  def diffBy1(str1: String, str2: String): Boolean = {
    (str1 zip str2).count { case (a, b) => a != b } == 1
  }

  def hasTwo(str: String) = counts(str).values.exists(_ == 2)
  def hasThree(str: String) = counts(str).values.exists(_ == 3)

  def counts(str: String): Map[Char, Int] =
    str.toList.groupBy(identity).mapValues(_.size)


  def parse(file: String) =
    Source.fromFile(file).getLines.toList

}
