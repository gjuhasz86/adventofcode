package aoc2017

import scala.io.Source

object Day01 {
  def main(args: Array[String]): Unit = {

    val input = Source.fromFile("src/main/resources/aoc2017/day01.txt").mkString

    test1()
    val res1 = ex1(input)
    val res2 = ex2(input)
    println(s"RES1: $res1")
    println(s"RES2: $res2")

  }

  def test1() = {
    val inputs = Vector("1122", "1111", "1234", "91212129")
    inputs.foreach { in =>
      val r = ex1(in)
      println(s" IN: $in")
      println(s"OUT: $r")
      println("---")
    }
  }

  def ex1(input: String): Int = {
    ex(input, 1)
  }

  def ex2(input: String): Int = {
    ex(input, input.length / 2)
  }

  def ex(input: String, shift: Int): Int = {
    val vec1 = input.map(_.toString.toInt).toVector
    val vec2 = vec1.drop(shift) ++ vec1.take(shift)

    val res = (vec1 zip vec2)
      .collect { case (a, b) if (a == b) => a }
      .sum

    res
  }

}
