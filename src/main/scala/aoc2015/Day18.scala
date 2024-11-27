package aoc2015

import scala.io.Source

object Day18 extends App {

  case class Pos(r: Int, c: Int) {
    val tup = (r, c)

    def neighbors(rows: Int, cols: Int): List[Pos] = {
      val allPos = List(
        Pos(r + 1, c),
        Pos(r - 1, c),
        Pos(r, c + 1),
        Pos(r, c - 1),
        Pos(r + 1, c + 1),
        Pos(r + 1, c - 1),
        Pos(r - 1, c + 1),
        Pos(r - 1, c - 1))
      allPos.filter(p => p.r >= 0 && p.c >= 0 && p.r < rows && p.c < cols)
    }
    override def toString: String = s"($r,$c)"
  }

  type CharBoard = Vector[Vector[Char]]
  implicit class RichBoard(b: CharBoard) {
    def get(p: Pos): Char = get(p.r, p.c)
    def get(r: Int, c: Int): Char = b(r)(c)
    def isOpen(p: Pos): Boolean = get(p.r, p.c) != '#'
    def mark(r: Int, c: Int, ch: Char): CharBoard =
      if (r >= 0 && c >= 0)
        b.updated(r, b(r).updated(c, ch))
      else b
    def mark(p: Pos, ch: Char): CharBoard = mark(p.r, p.c, ch)

    def rows = b.size
    def cols = b(0).size

    def p: String = b.map(r => r.mkString).mkString("\n")
    def pp: Unit = println(p)
  }


  def next(board0: CharBoard): CharBoard = {
    val board = stuck(board0)
    val lights = for {
      r <- 0 until board.rows
      c <- 0 until board.cols
    } yield {
      val pos = Pos(r, c)
      val neighbors = pos.neighbors(board.rows, board.cols)
      val onNeighbors = neighbors.count(p => board.get(p) == '#')
      val isOn = board.get(pos) == '#'
      val isOff = !isOn
      val shouldStayOn = onNeighbors == 2 || onNeighbors == 3
      val shouldTurnOn = onNeighbors == 3

      if ((isOn && shouldStayOn) || (isOff && shouldTurnOn))
        '#'
      else
        '.'
    }

    val newBoard = lights.toVector.grouped(board.cols).toVector
    val stuckBoard = stuck(newBoard)

    if (part1) newBoard else stuckBoard
  }

  def stuck(charBoard: CharBoard): CharBoard =
    charBoard
      .mark(0, 0, '#')
      .mark(0, charBoard.cols - 1, '#')
      .mark(charBoard.rows - 1, 0, '#')
      .mark(charBoard.rows - 1, charBoard.cols - 1, '#')

  val input0 =
    """.#.#.#
      |...##.
      |#....#
      |..#...
      |#.#..#
      |####..""".stripMargin.lines.toVector

  val input = Source.fromFile("src/main/scala/aoc2015/day18.txt").getLines.toVector

  val startBoard = input.map(_.toVector)

  def animate(charBoard: CharBoard, i: Int, steps: Int): CharBoard = {
    println()
    println(i)
    charBoard.pp
    if (steps <= 0) {
      charBoard
    } else {
      animate(next(charBoard), i + 1, steps - 1)
    }
  }

  val part1 = false
  val part2 = !part1

  val finalBoard = animate(startBoard, 0, 100)

  val on = finalBoard.flatten.count(_ == '#')
  println(on)

}
