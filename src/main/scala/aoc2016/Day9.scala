package aoc2016

import scala.io.Source

object Day9 extends App {
  val input = Source.fromFile("src/main/resources/aoc2016/day09.txt").mkString
  println(input)

  //  val input = "ADVENT A(1x5)BC (3x3)XYZ A(2x2)BCD(2x2)EFG (6x1)(1x3)A X(8x2)(3x3)ABCY"

  val v2 = true

  trait Data {
    def pretty: String
    def len: BigInt
  }

  case class Literal(str: String) extends Data {
    def pretty = str
    def len: BigInt = str.length
  }
  case class Rep(n: Int, data: Data) extends Data {
    def pretty = (1 to n).flatMap(_ => data.pretty).mkString
    def len: BigInt = data.len * n
  }
  case class Container(data: List[Data]) extends Data {
    def pretty = data.map(_.pretty).mkString
    def len: BigInt = data.map(_.len).sum
  }

  case class Marker(len: Int, n: Int)
  val MarkerRegex = """\(([0-9]+)x([0-9]+)\)""".r
  def parseMarker(str: String) = str match {
    case MarkerRegex(len, n) => Marker(len.toInt, n.toInt)
  }

  def parse(dataAcc: List[Data], acc: String, str: String): List[Data] = str.toList match {
    case Nil => dataAcc :+ Literal(acc)
    case ')' :: t =>
      val m = parseMarker(acc + ")")
      val current =
        if (v2)
          Rep(m.n, Container(parse(Nil, "", t.take(m.len).mkString)))
        else
          Rep(m.n, Literal(t.take(m.len).mkString))
      parse(dataAcc :+ current, "", t.drop(m.len).mkString)
    case '(' :: t => parse(dataAcc :+ Literal(acc), "(", t.mkString)
    case h :: t => parse(dataAcc, acc + h, t.mkString)
  }

  val parsed = parse(Nil, "", input)
  println(parsed)

  val decompressed = Container(parsed)
//  println(decompressed.pretty)
  println(decompressed.len)

  val len = parsed.map(_.len).sum
  println(len)
}
