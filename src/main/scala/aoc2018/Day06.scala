package aoc2018

import scala.io.Source

object Day06 {

  val testIn =
    """1, 1
      |1, 6
      |8, 3
      |3, 4
      |5, 5
      |8, 9""".stripMargin.lines.map(parseLine).toList

  def main(args: Array[String]): Unit = {
    val input = parse("src/main/resources/aoc2018/day06.txt")
    val res1 = ex1(input)
    println(s"RES1:$res1")
    val res2 = ex2(input)
    println(s"RES2:$res2")
  }

  def ex1(in: List[Pos]) = {
    val board = Board(in)
    val areas = board.fill
    val infinte = areas.filterKeys(board.onEdge).values.toSet

    val res =
      areas.values
        .filter(!infinte.contains(_))
        .groupBy(identity)
        .values
        .map(_.size)
        .max
    res
  }

  def ex2(in: List[Pos]) = {
    val board = Board(in)
    board.countSafe
  }

  case class Board(in: List[Pos]) {
    val minX = in.minBy(_.x).x
    val minY = in.minBy(_.y).y
    val maxX = in.maxBy(_.x).x
    val maxY = in.maxBy(_.y).y

    def onEdge(p: Pos) =
      p.x == minX || p.x == maxX || p.y == minY || p.y == maxY

    def fill = {
      def loop(pos: Pos, acc: Map[Pos, Option[Pos]]): Map[Pos, Option[Pos]] = {
        if (pos.y > maxY) {
          loop((pos.x + 1, minY), acc)
        } else if (pos.x > maxX) {
          acc
        } else {
          loop((pos.x, pos.y + 1), acc + (pos -> closest(pos)._1))
        }
      }

      loop((minX, minY), Map())
    }

    def closest(pos: Pos): (Option[(Int, Int)], Int) =
      in.foldLeft((None: Option[Pos], Int.MaxValue)) { case ((minPos, min), curr) =>
        val d = pos.dist(curr)
        if (d < min)
          (Some(curr), d)
        else if (d == min)
          (None, d)
        else
          (minPos, min)
      }

    def countSafe = {
      def loop(pos: Pos, acc: Int): Int = {
        if (pos.y > maxY) {
          loop((pos.x + 1, minY), acc)
        } else if (pos.x > maxX) {
          acc
        } else {
          val inc = if (isSafe(pos)) 1 else 0
          loop((pos.x, pos.y + 1), acc + inc)
        }
      }

      loop((minX, minY), 0)
    }


    def isSafe(pos: Pos): Boolean =
      in.foldLeft(0) { (acc, p) => acc + p.dist(pos) } < 10000

    def XXX(p: Pos) = {
      var min = Int.MaxValue
      var minPos: Pos = null
      var invalid = false
      (minX to maxX).foreach { x =>
        (minY to maxY).foreach { y =>
          val d = (x, y).dist(p)
          if (d < min) {
            min = d
            minPos = (x, y)
            invalid = false
          } else if (d == min) {
            invalid = true
          }
        }
      }
      if (invalid) None else Some(minPos)
    }
  }

  def parse(file: String) =
    Source.fromFile(file).getLines.map(parseLine).toList

  def parseLine(str: String) = {
    val Array(a, b) = str.split(", ")
    (a.toInt, b.toInt)
  }

  type Pos = (Int, Int)

  implicit class PosOps(val p: Pos) extends AnyVal {
    def x = p._1
    def y = p._2
    def dist(other: Pos) = Math.abs(p.x - other.x) + Math.abs(p.y - other.y)
  }
}
