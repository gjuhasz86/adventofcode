package aoc2015

import scala.io.Source

object Day3 extends App {
  val input = Source.fromFile("src/main/scala/aoc2015/day3.txt").mkString
  println(input)

  //  val input = "^>v<"

  case class Pos(x: Int, y: Int) {
    def step(c: Char): Pos = c match {
      case '^' => this.copy(x = x - 1)
      case 'v' => this.copy(x = x + 1)
      case '<' => this.copy(y = y - 1)
      case '>' => this.copy(y = y + 1)
    }
  }
  def part1(): Unit = {

    def walk(acc: List[Pos], p: Pos, moves: List[Char]): List[Pos] = moves match {
      case Nil => p :: acc
      case h :: t => walk(p :: acc, p.step(h), t)
    }

    val path = walk(Nil, Pos(0, 0), input.toList)
    println(path)

    val visited = path.distinct.size
    println(visited)
  }
  part1()

  def part2(): Unit = {


    case class DoublePos(sPos: Pos, rPos: Pos, robo: Boolean) {
      def step(c: Char): DoublePos =
        if (robo)
          this.copy(rPos = rPos.step(c), robo = false)
        else
          this.copy(sPos = sPos.step(c), robo = true)
    }

    def walk(acc: List[Pos], p: DoublePos, moves: List[Char]): List[Pos] = moves match {
      case Nil => p.rPos :: p.sPos :: acc
      case h :: t => walk(p.rPos :: p.sPos :: acc, p.step(h), t)
    }

    val path = walk(Nil, DoublePos(Pos(0, 0), Pos(0, 0), false), input.toList)
    println(path)

    val visited = path.distinct.size
    println(visited)

  }
  part2()

}
