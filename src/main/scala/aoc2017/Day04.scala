package aoc2017

import scala.io.Source

object Day04 {
  def main(args: Array[String]): Unit = {
    val input = parse("src/main/resources/aoc2017/day04.txt")

    val res1 = ex1(input)
    val res2 = ex2(input)

    println(s"RES1: $res1")
    println(s"RES2: $res2")
  }

  def ex1(in: Vector[Vector[String]]): Int = {
    in.count(valid1)
  }

  def ex2(in: Vector[Vector[String]]): Int = {
    in.count(valid2)
  }

  def valid1(words: Vector[String]) =
    words.groupBy(identity).mapValues(_.size).values.forall(_ == 1)

  def valid2(words: Vector[String]) =
    valid1(words.map(_.sorted))

  def parse(file: String): Vector[Vector[String]] =
    Source.fromFile(file).getLines.map(_.split(" ").toVector).toVector
}
