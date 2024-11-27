package aoc2016

object Day18 extends App {
  val input = "^..^^.^^^..^^.^...^^^^^....^.^..^^^.^.^.^^...^.^.^.^.^^.....^.^^.^.^.^.^.^.^^..^^^^^...^.....^....^."

  def next(str: String): String = {
    val ext = "." + str + "."
    val newRow = ext.sliding(3).map {
      case "^^." | ".^^" | "^.." | "..^" => "^"
      case _ => "."
    }
    newRow.mkString
  }

  def room(acc: List[String], rows: Int): List[String] =
    if (rows <= 1) {
      acc
    } else {
      println(rows)
      val nextRow = next(acc.last)
      room(acc :+ nextRow, rows - 1)
    }

  val res1 = room(input :: Nil, 40)
  res1 foreach println
  val resPart1 = res1.flatten.count(_ == '.')
  println(resPart1)

  def roomSafeCount(acc: BigInt, last: String, rows: Int): BigInt = {
    val safeTileCount = last.count(_ == '.')
    if (rows <= 1) {
      acc + safeTileCount
    } else {
      if (rows % 100 == 0) {
        println(rows)
      }
      val nextRow = next(last)
      roomSafeCount(acc + safeTileCount, nextRow, rows - 1)
    }
  }

  val resPart2 = roomSafeCount(0, input, 400000)
  println(resPart2)

  println()
  println(s"PART1: $resPart1")
  println(s"PART2: $resPart2")


}
