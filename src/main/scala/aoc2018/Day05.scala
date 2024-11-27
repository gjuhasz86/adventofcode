package aoc2018

import scala.io.Source

object Day05 {
  val input = parse("src/main/resources/aoc2018/day05.txt")
  val testIn = "dabAcCaCBAcCcaDA"

  def main(args: Array[String]): Unit = {
    val res1 = ex1(input)
    println(s"RES1: $res1")

    val res2 = ex2(input)
    println(s"RES2: $res2")
  }

  val diff = Math.abs('a' - 'A')

  def ex1(pmr: String): Int = {
    val r1 = react(pmr.grouped(2)) // even positions
    val r2 = r1.head.toString + react(r1.tail.grouped(2)) // odd positions
    if (r2 == pmr) r2.length else ex1(r2)
  }

  def ex2(orig: String) =
    orig.toLowerCase.toSet // set of unit types used
      .map { (ch: Char) =>
      val transformed = orig.filter(c => c != ch && c != ch - diff)
      ex1(transformed)
    }.min

  def react(pairs: Iterator[String]) =
    pairs.map {
      case s if canReact(s) => ""
      case s => s
    }.mkString

  def canReact(s: String): Boolean =
    s.length == 2 && (Math.abs(s(0) - s(1)) == diff)

  def parse(file: String) =
    Source.fromFile(file).mkString

}
