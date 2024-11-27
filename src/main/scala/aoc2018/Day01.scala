package aoc2018

import scala.io.Source

object Day01 {

  def parse(file: String): Vector[Int] =
    Source.fromFile(file).getLines.map(_.toInt).toVector

  val testIn = List(1, -2, 3, 1)

  def main(args: Array[String]): Unit = {
    val input = parse("src/main/resources/aoc2018/day01.txt")

    val res1 = ex1(input)
    println(s"RES1: $res1")
    //    val res2 = ex2(input.toList)
    val res2 = ex2(input.toList)
    println(s"RES2: $res2")
  }

  def ex1(in: Vector[Int]) = {
    in.sum
  }

  def ex2(in: List[Int]) = {

    def loop(visited: Set[Int], curr: Int, list: List[Int]): Int =
      if (visited.contains(curr))
        curr
      else
        list match {
          case h :: Nil => loop(visited + curr, curr + h, in)
          case h :: t => loop(visited + curr, curr + h, t)
          case Nil => ???
        }

    loop(Set(), 0, in)
  }
}
