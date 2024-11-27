package aoc2016

import scala.io.Source

object Day7 extends App {

  val input = Source.fromFile("src/main/resources/aoc2016/day07.txt").getLines.toList
  println(input)

  def part1() {

    //  val input = List(
    //    "abba[mnop]qrst",
    //    "abcd[bddb]xyyx",
    //    "aaaa[qwer]tyui",
    //    "ioxxoj[asdfgh]zxcvbn"
    //  )

    val abbaRe = ".*([a-z])((?:(?!\\1).))\\2\\1.*"

    val res = input.map { case ip =>
      val parts = ip.split("[\\[\\]]").toList.zipWithIndex
      val (standard, hyper) = parts.partition(_._2 % 2 == 0)
      val ok = standard.exists(_._1.matches(abbaRe)) &&
        hyper.forall(!_._1.matches(abbaRe))
      (ok, ip)
    }
    println(res)
    println(res.filter(_._1).size)
  }
  part1()

  def part2() {

    //    val input = List(
    //      "aba[bab]xyz",
    //      "xyx[xyx]xyx",
    //      "aaa[kek]eke",
    //      "zazbz[bzb]cdb"
    //    )

    val abaRe = "([a-z])((?:(?!\\1).))\\1".r

    val getAbas = (str: String) => str.scanRight("")((c, a) => c + a).flatMap { x => abaRe.findFirstIn(x).toList }.distinct
    val invert = (str: String) => s"${str(1)}${str(0)}${str(1)}"
    val res = input.map { case ip =>
      val parts = ip.split("[\\[\\]]").toList.zipWithIndex
      val (standard, hyper) = parts.partition(_._2 % 2 == 0)
      val abas = standard.map(_._1).flatMap(getAbas).map(invert)
      val babs = hyper.map(_._1).flatMap(getAbas)
      println(s"ABA: $abas")
      println(s"BAB: $babs")
      val r = abas intersect babs
      println(r)
      (r.nonEmpty, ip)
    }
    println(res)
    println(res.filter(_._1).size)
  }
  part2()
}
