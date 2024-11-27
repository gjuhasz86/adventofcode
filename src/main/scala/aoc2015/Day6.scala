package aoc2015

import javax.naming.ldap.PagedResultsControl

import scala.io.Source

object Day6 extends App {
  val input = Source.fromFile("src/main/scala/aoc2015/day6.txt").getLines().toList
  println(input)

  type Op = Boolean => Boolean
  val On = (b: Boolean) => true
  val Off = (b: Boolean) => false
  val Toggle = (b: Boolean) => !b

  case class Pos(x: Int, y: Int)

  case class Instruction(op: Op, from: Pos, to: Pos)

  val InstructionRegex = """^(.+?) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$""".r

  def parse(s: String) = {
    val InstructionRegex(opStr, fromX, fromY, toX, toY) = s
    val op = opStr match {
      case "turn on" => On
      case "turn off" => Off
      case "toggle" => Toggle
    }
    Instruction(op, Pos(fromX.toInt, fromY.toInt), Pos(toX.toInt, toY.toInt))
  }

  val parsed = input.map(parse)
  parsed foreach println


  def inRange(x: Int, y: Int, from: Pos, to: Pos) =
    from.x <= x && x <= to.x && from.y <= y && y <= to.y

  val size = 10
  def toPos(i: Int) = (i / size, i % size)

  case class Board(lights: List[Boolean]) {
    def switch(in: Instruction) = Board {
      for {
        (light, i) <- lights.zipWithIndex
      } yield {
        val (x, y) = toPos(i)
        if (inRange(x, y, in.from, in.to))
          in.op(light)
        else
          light
      }
    }

    override def toString: String = lights.grouped(size).toList.map(_.map(b => if (b) 'o' else '_').mkString + '\n').mkString
  }

  val startBoard = Board((1 to size * size).toList.map(_ => false))

  val testIns = List(
    Instruction(On, Pos(2, 2), Pos(3, 3)),
    Instruction(On, Pos(3, 3), Pos(4, 4)),
    Instruction(Toggle, Pos(2, 2), Pos(4, 4))
  )

  val r = testIns.foldLeft(startBoard) { (b, i) => b.switch(i) }
  println(r)
  println(r.lights.count(_ == true))

}

object Day6Part2 extends App {
  val input = Source.fromFile("src/main/scala/aoc2015/day6.txt").getLines().toList
  println(input)

  type Op = Int => Int
  val On = (b: Int) => b + 1
  val Off = (b: Int) => if (b == 0) 0 else b - 1
  val Toggle = (b: Int) => b + 2

  case class Pos(x: Int, y: Int)

  case class Instruction(op: Op, from: Pos, to: Pos)

  val InstructionRegex = """^(.+?) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$""".r

  def parse(s: String) = {
    val InstructionRegex(opStr, fromX, fromY, toX, toY) = s
    val op = opStr match {
      case "turn on" => On
      case "turn off" => Off
      case "toggle" => Toggle
    }
    Instruction(op, Pos(fromX.toInt, fromY.toInt), Pos(toX.toInt, toY.toInt))
  }

  val parsed = input.map(parse)
  parsed foreach println


  def inRange(x: Int, y: Int, from: Pos, to: Pos) =
    from.x <= x && x <= to.x && from.y <= y && y <= to.y

  val size = 1000
  def toPos(i: Int) = (i / size, i % size)

  case class Board(lights: List[Int]) {
    def switch(in: Instruction) = Board {
      for {
        (light, i) <- lights.zipWithIndex
      } yield {
        val (x, y) = toPos(i)
        if (inRange(x, y, in.from, in.to))
          in.op(light)
        else
          light
      }
    }

    override def toString: String = lights.grouped(size).toList.map(_.mkString + '\n').mkString
  }

  val startBoard = Board((1 to size * size).toList.map(_ => 0))

  val testIns = List(
    Instruction(On, Pos(2, 2), Pos(3, 3)),
    Instruction(Off, Pos(0, 0), Pos(2, 2)),
    Instruction(On, Pos(3, 3), Pos(4, 4)),
    Instruction(Toggle, Pos(2, 2), Pos(4, 4))
  )

  val r = parsed.foldLeft(startBoard) { (b, i) => b.switch(i) }
  println(r)
  println(r.lights.sum)

}
