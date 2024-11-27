package aoc2016

import scala.io.Source

object Day6 extends App {

  val input = Source.fromFile("src/main/resources/aoc2016/day06.txt").getLines().toList

  //  val input =
  //    """eedadn
  //      |drvtee
  //      |eandsr
  //      |raavrd
  //      |atevrs
  //      |tsrnev
  //      |sdttsa
  //      |rasrtv
  //      |nssdts
  //      |ntnada
  //      |svetve
  //      |tesnvt
  //      |vntsnd
  //      |vrdear
  //      |dvrsen
  //      |enarar""".stripMargin.lines.toList


  val code = input.transpose.map(ls => ls.map(l => (l, ls.count(_ == l))).maxBy(_._2)).map(_._1).mkString
  println(code)

  val code2 = input.transpose.map(ls => ls.map(l => (l, ls.count(_ == l))).minBy(_._2)).map(_._1).mkString
  println(code2)
}
