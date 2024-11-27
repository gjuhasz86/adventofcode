package aoc2017

import scala.io.Source

object Day05 {
  def main(args: Array[String]): Unit = {
    val input = parse("src/main/resources/aoc2017/day05.txt")

    test1()
    val res1 = ex1(input)
    println(s"RES1: $res1")
    println()
    test2()
    val res2 = ex2(input)
    println(s"RES2: $res2")

  }

  def test1() = {
    val res = ex1(Vector(0, 3, 0, 1, -3))
    println(s"TEST1: $res")
  }

  def test2() = {
    val res = ex2(Vector(0, 3, 0, 1, -3))
    println(s"TEST2: $res")
  }

  def ex1(in: Vector[Int]) = follow1(in, 0, 0)
  def ex2(in: Vector[Int]) = follow2(in, 0, 0)

  def follow1(vec: Vector[Int], pos: Int, counter: Int): Int = {
    if (vec.indices.contains(pos)) {
      //      println(vec.zipWithIndex.map { case (x, i) => if (i == pos) s"($x)" else x }.mkString(" "))
      val jmp = vec(pos)
      follow1(vec.updated(pos, jmp + 1), pos + jmp, counter + 1)
    } else {
      counter
    }
  }

  def follow2(vec: Vector[Int], pos: Int, counter: Int): Int = {
    if (vec.indices.contains(pos)) {
      //      println(vec.zipWithIndex.map { case (x, i) => if (i == pos) s"($x)" else x }.mkString(" "))
      val jmp = vec(pos)
      val newJmp = if (jmp >= 3) jmp - 1 else jmp + 1
      follow2(vec.updated(pos, newJmp), pos + jmp, counter + 1)
    } else {
      counter
    }
  }

  def parse(file: String): Vector[Int] =
    Source.fromFile(file).getLines.map(_.toInt).toVector
}
