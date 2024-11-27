package aoc2016

import scala.io.Source

object Day4 extends App {
  // 09:17:30
  // 09:51:00 ~ 00:34:30

  val input = Source.fromFile("src/main/resources/aoc2016/day04.txt").getLines().toList
  println(input)

  //  val input = List(
  //    "aaaaa-bbb-z-y-x-123[abxyz]",
  //    "a-b-c-d-e-f-g-h-987[abcde]",
  //    "not-a-real-room-404[oarel]",
  //    "totally-real-room-200[decoy]"
  //  )

  val CodeRegex = """([a-z-]+)([0-9]+)\[(.+)\]""".r

  case class Data(name: String, secId: Int, chk: String)
  def parse(s: String) = {
    val CodeRegex(name, secId, chk) = s
    Data(name.replaceAll("-", ""), secId.toInt, chk)
  }

  val parsed = input.map(parse)
  println(parsed)

  def chkGen(str: String) = str.groupBy(identity).toList
    .groupBy(_._2.length).toList
    .sortBy(_._1 * -1)
    .map(_._2)
    .map(_.sorted)
    .flatten.map(_._1)
    .take(5)
    .mkString

  val checked = parsed zip parsed.map(d => chkGen(d.name))
  checked foreach println

  val valids = checked.collect { case (d, chk) if d.chk == chk => d }
  val secIdSum = valids.map(_.secId).sum
  println(secIdSum)

  def rotate(c: Char, n: Int): Char = ((c.toInt + n - 97) % 26 + 97).toChar
  def rotate(d: Data): String = d.name.map(c => rotate(c, d.secId))

  val rotated = valids.map(d => rotate(d)) zip valids
  rotated foreach println

  println()
  val northpole = rotated.filter(_._1.contains("north"))
  println(northpole)
}
