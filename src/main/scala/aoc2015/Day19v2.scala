package aoc2015

import scala.io.Source

object Day19v2 extends App {
  implicit class RegexFoo(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  val input0 = Source.fromFile("src/main/scala/aoc2015/day19.txt").getLines.toList

  val input1 =
    """H => HO
      |H => OH
      |O => HH
      |
      |HOH""".stripMargin.lines.toList

  val input =
    """e => H
      |e => O
      |H => HO
      |H => OH
      |O => HH
      |
      |HOHOHO""".stripMargin.lines.toList


  case class Rule(from: String, to: String) {
    def use(str: String): List[String] = use0(str, 0, Nil)
    private def use0(str: String, n: Int, acc: List[String]): List[String] = {
      val idx = str.indexOf(from, n)
      if (idx == -1) {
        acc
      } else {
        val applied = str.take(idx) + to + str.drop(idx + from.length)
        use0(str, idx + 1, applied :: acc)
      }
    }

    def range(n: Int): Range = n until n + from.length

    def allRanges(str: String): List[Range] =
      allIndices(str).map(range)

    def allIndices(str: String): List[Int] =
      (0 until str.length).filter(str.startsWith(from, _)).toList

    def reversed: Rule = Rule(to, from)
  }

  def parse(str: String): Rule = str match {
    case r"(.+)$f => (.+)$t" => Rule(f, t)
  }

  val rules = input.dropRight(2).map(parse)
  val medicine = input.last


}
