package aoc2015

import scala.io.Source

object Day5 extends App {
  val input = Source.fromFile("src/main/scala/aoc2015/day5.txt").getLines().toList
  println(input)
  //  val input = List("ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb")
  //  val input = List("qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg", "ieodomkazucvgmuy")

  val vowels = "aeiou"

  def hasDouble(s: String): Boolean =
    (s.toList zip s.toList.drop(1)).exists { case (a, b) => a == b }


  def isNice(s: String) =
    s.toList.filter(c => vowels.contains(c.toString)).size > 2 &&
      hasDouble(s) &&
      !s.contains("ab") &&
      !s.contains("cd") &&
      !s.contains("pq") &&
      !s.contains("xy")


  val checked = input zip input.map(isNice)
  checked foreach println

  println(checked.count(_._2))


  def hasDouble(s: List[Char]): Boolean = s match {
    case a :: b :: rest =>
      val pat = List(a, b).mkString
      //      println(s"[$pat] [$rest]")
      if (rest.mkString.contains(pat)) true else hasDouble(b :: rest)
    case a :: rest => false
    case Nil => false
  }

  def hasRepeat(s: List[Char]): Boolean = s.sliding(3).toList.exists {
    case List(a, b, c) => a == c
    case _ => false
  }
  def isNice2(s: String) = hasDouble(s.toList) && hasRepeat(s.toList)

  val checked2 = input zip input.map(isNice2)
  checked2 foreach println

  println(checked2.count(_._2))

}
