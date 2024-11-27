package aoc2017

import scala.io.Source

object Day02 {

  def main(args: Array[String]): Unit = {
    val input = parse("src/main/resources/aoc2017/day02.txt")


    test2()
    val res1 = ex1(input)
    val res2 = ex2(input)
    println(s"RES1: $res1")
    println(s"RES2: $res2")
  }


  def test2() = {
    val in = Vector(
      Vector(5, 9, 2, 8),
      Vector(9, 4, 7, 3),
      Vector(3, 8, 6, 5))
    val r = ex2(in)
    println(s"TEST2: $r")
  }

  def ex1(input: Vector[Vector[Int]]) = {
    input.map(row => row.max - row.min).sum
  }
  def ex2(input: Vector[Vector[Int]]) = {
    input.map { row =>
      val res = for {a <- row; b <- row}
        yield if (a % b == 0 && a != b) Some(a/ b) else None
//      println(res.flatten)
      res.flatten.sum
    }.sum
  }


  def parse(file: String): Vector[Vector[Int]] =
    Source.fromFile(file).getLines.map(_.split("\t").map(_.toInt).toVector).toVector

}
