package aoc2015

import scala.io.Source

object Day2 extends App {
  val input = Source.fromFile("src/main/scala/aoc2015/day2.txt").getLines.toList
  println(input)

  //  val input = "2x3x4" :: Nil

  def parse(str: String) = str.trim.split("x").toList.map(_.toInt) match {
    case Seq(a, b, c) => (a, b, c)
  }
  val parsed = input.map(parse)
  println(parsed)

  def areas(w: Int, h: Int, l: Int): Seq[Int] =
    Seq(w * h, w * h, h * l, h * l, w * l, w * l)

  val areas0 = parsed.map((areas _).tupled)
  println(areas0)

  def paperSize(w: Int, h: Int, l: Int): Int =
    areas(w, h, l).sum + areas(w, h, l).min

  val total = parsed.map((paperSize _).tupled)
  println(total)
  println(total.sum)

  def circumferences(w: Int, h: Int, l: Int): Seq[Int] =
    Seq(w + h + w + h, h + l + h + l, w + l + w + l)

  def ribbonSize(w: Int, h: Int, l: Int): Int =
    circumferences(w, h, l).min + w * h * l

  val totalRibbon = parsed.map((ribbonSize _).tupled)
  println(totalRibbon)
  println(totalRibbon.sum)
}
