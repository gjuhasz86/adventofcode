package aoc2016

import scala.io.Source

object Day8 extends App {

  val input = Source.fromFile("src/main/resources/aoc2016/day08.txt").getLines.toList
  println(input)

  //  val input = List(
  //    "rect 3x2",
  //    "rotate column x=1 by 1",
  //    "rotate row y=0 by 4",
  //    "rotate column x=1 by 1"
  //  )

  val RectRegex = """rect (.+)x(.+)""".r
  val RotColRegex = """rotate column x=(.+) by (.+)""".r
  val RotRowRegex = """rotate row y=(.+) by (.+)""".r

  sealed trait Op
  case class Rect(x: Int, y: Int) extends Op
  case class RotateCol(x: Int, by: Int) extends Op
  case class RotateRow(y: Int, by: Int) extends Op

  def parse(str: String) = str match {
    case RectRegex(x, y) => Rect(x.toInt, y.toInt)
    case RotColRegex(x, by) => RotateCol(x.toInt, by.toInt)
    case RotRowRegex(y, by) => RotateRow(y.toInt, by.toInt)
  }

  val ops = input map parse
  ops foreach println


  def rotate(list: List[Boolean], n: Int): List[Boolean] = (list, n) match {
    case (l, 0) => l
    case (Nil, _) => Nil
    case (h :+ l, n) => rotate(l :: h, n - 1)
  }

  case class Display(leds: List[List[Boolean]]) {
    def rect(c: Int, r: Int): Display = Display {
      leds.zipWithIndex.map { case (row, ri) =>
        row.zipWithIndex.map { case (led, ci) =>
          if (ci < c && ri < r) true else led
        }
      }
    }
    def rotateRow(r: Int, by: Int): Display = Display {
      leds.zipWithIndex.map { case (row, ri) =>
        if (ri == r) rotate(row, by) else row
      }
    }
    def rotateCol(c: Int, by: Int): Display = Display {
      Display(leds.transpose).rotateRow(c, by).leds.transpose
    }

    def apply(op: Op): Display = op match {
      case Rect(x, y) => rect(x, y)
      case RotateCol(x, by) => rotateCol(x, by)
      case RotateRow(y, by) => rotateRow(y, by)
    }

    def lit: Int = leds.flatten.count(identity)
    override def toString: String = leds.map { row => row.map(l => if (l) '#' else '.').mkString + "\n" }.mkString
  }

  object Display {
    def apply(x: Int, y: Int): Display = Display {
      (1 to y).map(_ => (1 to x).map(_ => false).toList).toList
    }
  }

  val disp = Display(50, 6)
  println(disp)


  println()
  val result = ops.foldLeft(disp)((d, op) => d.apply(op))
  println(result)
  println(result.lit)

}
